package view

import com.typesafe.scalalogging.Logger
import common.*
import controller.CompetenciesController
import tui.Alignment
import tui.Borders
import tui.Color
import tui.Constraint
import tui.Direction.Horizontal
import tui.Frame
import tui.Layout
import tui.Modifier
import tui.Rect
import tui.Span
import tui.Spans
import tui.Style
import tui.Text
import tui.Widget
import tui.crossterm.KeyCode
import tui.widgets.BlockWidget
import tui.widgets.BlockWidget.BorderType
import tui.widgets.ListWidget
import tui.widgets.ParagraphWidget
import view.CompetenciesView.Focus
import view.CompetenciesView.Focus.Popup
import view.CompetenciesView.PopupType
import view.CompetenciesView.SelectedItemStyle
import view.CompetenciesView.ViewState

val log: Logger = Logger(classOf[CompetenciesView])

class CompetenciesView private (
    val cntrl: CompetenciesController,
    val state: ViewState,
    val competenciesLayout: Layout,
    val maxNestLevel: Int,
    val show: MessageShow
) extends TuiView:

  override def handledKeyboard(key: KeyCode): Option[CompetenciesView] = state.focused match
    case Focus.Competencies => Some(handledCompetenciesInput(key))
    case Focus.QAs          => Some(handledQAsInput(key))
    case Popup(kind, _) =>
      kind match
        case PopupType.CompetencyEstimate => Some(handledCompetencyEstimationInput(key))
        case PopupType.QAsEstimate        => Some(handledQAsEstimationInput(key))

  private def handledCompetenciesInput(key: KeyCode): CompetenciesView =
    key match
      // Arrows
      case _: KeyCode.Up    => prevCompetencySelected.getOrElse(this)
      case _: KeyCode.Down  => nextCompetencySelected.getOrElse(this)
      case _: KeyCode.Left  => parentCompetencySelected.getOrElse(this)
      case _: KeyCode.Right => childCompetencySelected.getOrElse(this)

      // Open popup input for competency estimation, just change focus
      case _: KeyCode.Enter => focusChanged(Focus.Popup(PopupType.CompetencyEstimate))

      // Erase competency estimate if exists
      case _: KeyCode.Delete =>
        cntrl.estimatedCompetency(state.selected.competency, KnowledgeCompleteness.NotMentioned)
        this

      // Tab key changes focus
      case _: KeyCode.Tab => focusChanged(Focus.QAs)

      case _ => this
  end handledCompetenciesInput

  private def childCompetencySelected: Option[CompetenciesView] =
    val childs = childCompetencies
    if childs.isEmpty then None
    else childs.headOption.map(select => withState(state.childSelected(select.numeration)))

  private def childCompetencies: Seq[CompetencyView] = cntrl.competencies
    .filter(c => c.numeration.directParent.map(p => p == state.selected.competency).getOrElse(false))

  private def parentCompetencySelected: Option[CompetenciesView] =
    if state.selectionContext.nonEmpty then Some(withState(state.parentSelected)) else None

  private def nextCompetencySelected: Option[CompetenciesView] = cntrl.competencies
    .find(c => c.numeration == state.selected.competency.next)
    .map(next => withState(state.neighbourSelected(next.numeration)))

  private def prevCompetencySelected: Option[CompetenciesView] = cntrl.competencies
    .find(c => c.numeration == state.selected.competency.previous)
    .map(next => withState(state.neighbourSelected(next.numeration)))

  private def withState(s: ViewState) = CompetenciesView(s)(using cntrl, show)

  private def focusChanged(resultFocus: Focus): CompetenciesView =
    (state.focused, resultFocus) match
      case (Focus.Competencies, Focus.QAs) => changeFocusBetweenQACompetency(resultFocus)
      case (Focus.QAs, Focus.Competencies) => changeFocusBetweenQACompetency(resultFocus)
      // From popup to Competency or QAs
      // Or from Competency or QAs to popup
      case _ => withState(state.focusedOn(resultFocus))

  private def changeFocusBetweenQACompetency(focus: Focus): CompetenciesView =
    // Init selected qa with first item in current competency
    val selectedQAIndex = (currentCompetency, state.selected.qaIndex) match
      case (competency, None) => Option.when(competency.questions.nonEmpty)(0)
      case (_, Some(sel))     => Some(sel)

    selectedQAIndex match
      case None             => this // Do not change focus - there are no qa items
      case Some(selectedQA) => withState(state.focusedOn(focus).qaSelected(selectedQA))

  private def handledQAsInput(key: KeyCode): CompetenciesView =
    key match
      // Arrows
      case _: KeyCode.Up   => prevQASelected.getOrElse(this)
      case _: KeyCode.Down => nextQASelected.getOrElse(this)

      // Open popup input for QA estimation, just change focus
      case _: KeyCode.Enter => focusChanged(Focus.Popup(PopupType.QAsEstimate))

      // Tab key changes focus
      case _: KeyCode.Tab => withState(state.focusedOn(Focus.Competencies))

      case _ => this

  private def nextQASelected: Option[CompetenciesView] =
    val qa: Seq[QA] = currentCompetency.questions
    val nextIdx: Option[Int] = state.selected.qaIndex
      .map(currIdx => (qa.size - 1).min(currIdx + 1))
      .orElse(Option.when(qa.nonEmpty)(0)) // or first idx if exists

    nextIdx.map(newIdx => state.qaSelected(newIdx)).map(withState(_))

  private def prevQASelected: Option[CompetenciesView] =
    val prevIdx: Option[Int] = state.selected.qaIndex
      .map(currIdx => 0.max(currIdx - 1))
      .orElse(Option.when(currentCompetency.questions.nonEmpty)(0)) // or first idx if exists

    prevIdx.map(newIdx => state.qaSelected(newIdx)).map(withState(_))

  override def render(frame: Frame, at: Rect) =
    val layout = Layout(
      direction = Horizontal,
      constraints = Array(Constraint.Percentage(60), Constraint.Percentage(40))
    )
    val chunks = layout.split(at)
    require(chunks.size == 2)

    renderCompetencies(frame, chunks(0))
    renderQA(frame, chunks(1))
    renderPopup(frame, popupRect(at))

  private def renderCompetencies(frame: Frame, at: Rect) =
    val chunks: Array[Rect] = competenciesLayout.split(at)
    require(chunks.size == maxNestLevel + 1, s"Chunks size=${chunks.size}, maxNestLevel=${state.nestLevel}")

    // TODO Ideally, we should render only 3 levels: previous, current and last, and use special symbols to show that
    // you can go lower / higher in the hierarchy. If we don't do this, then with a large nesting of competencies, the
    // render will be broken, all competencies will be cut off by the composer
    for level <- 0 to maxNestLevel do
      val borders = level match
        case 0                          => Borders.LEFT | Borders.TOP | Borders.BOTTOM
        case lvl if lvl == maxNestLevel => Borders.RIGHT | Borders.TOP | Borders.BOTTOM
        case _                          => Borders.TOP | Borders.BOTTOM
      renderCompetencyLevel(frame, chunks(level), borders, level, cntrl.computedKnowledges)

  private def renderCompetencyLevel(
      frame: Frame,
      at: Rect,
      borders: Borders,
      nestLevel: Int,
      knowledges: Map[Numeration, KnowledgeComputed]
  ) =
    val competenciesToRender: Array[ListWidget.Item] = toRenderCompetencies(nestLevel)
      .map(c => competencyListItem(c, nestLevel, knowledges, at))
      .toArray

    val widgetTitle = if nestLevel == 0 then Some(Spans.nostyle("Competencies")) else None
    val widget = ListWidget(
      items = competenciesToRender,
      block = Some(BlockWidget(borders = borders, title = widgetTitle))
    )
    frame.renderWidget(widget, at)

  private def competencyListItem(
      c: CompetencyView,
      nestLevel: Int,
      knowledges: Map[Numeration, KnowledgeComputed],
      at: Rect
  ): ListWidget.Item =
    val selectedStyle =
      if state.focused == Focus.Competencies && state.selected.competency == c.numeration then SelectedItemStyle
      else Style.DEFAULT
    val header = Span.styled(CompetenciesView.competencyHeaderText(c, knowledges), selectedStyle).bounded(at)
    ListWidget.Item(content = header)

  private def competenciesAtLevel(level: Int): Seq[CompetencyView] =
    CompetenciesView.competenciesAtLevel(cntrl.competencies, level)

  private def toRenderCompetencies(level: Int): Seq[CompetencyView] =
    val atLevel = competenciesAtLevel(level)
    if level > state.nestLevel + 1 then
      // Do not render transitive childs
      List.empty
    else if level == state.nestLevel + 1 then
      // Render only childs of current selection
      atLevel.filter(c => c.numeration.isChildOf(state.selected.competency))
    else if level == 0 then
      // Render all cuz items at root should be shown as-is
      atLevel
    else
      // Render only direct childs of previous level selected items
      val prevLevelSelected = state.findSelectedAtLevel(level - 1).get
      atLevel.filter(c => c.numeration.isChildOf(prevLevelSelected))

  private def renderQA(frame: Frame, at: Rect) =
    val allQA = currentCompetency.questions
    val qaItems = (0 until allQA.size)
      .map(idx => allQA(idx) -> idx)
      .map((qa, qaIdx) => qaListItem(qa, qaIdx, at))
      .toArray
    val widgetTitle = Some(Spans.nostyle("QAs"))
    val widgetBlock = Some(BlockWidget(borders = Borders.ALL, borderType = BorderType.Rounded, title = widgetTitle))
    val widget = ListWidget(items = qaItems, block = widgetBlock)
    frame.renderWidget(widget, at)

  private def qaListItem(qa: QA, qaIdx: Int, at: Rect): ListWidget.Item =
    val selectedStyle =
      if state.focused == Focus.QAs && state.selected.qaIndex.getOrElse(-1) == qaIdx then SelectedItemStyle
      else Style.DEFAULT
    val q = Some(Span.styled(s"Q: ${qa.questionBody}", selectedStyle).bounded(at))
    val a = qa.answerBody.map(ans => s"A: ${ans}").map(ans => Span.styled(ans, selectedStyle).bounded(at))
    val qaText = Array(q, a).collect(_ match { case Some(txt) => txt }).reduceLeft((left, right) => left.concat(right))
    ListWidget.Item(content = qaText)

  private def renderPopup(frame: Frame, at: Rect) =
    val activePopup: Option[Popup] = state.focused match
      case popup @ Popup(_, _) => Some(popup)
      case _                   => None

    val widget: Option[Widget] = activePopup.map: popup =>
      popup.kind match
        case PopupType.CompetencyEstimate => popupCompetencyEstimateWidget(popup)
        case PopupType.QAsEstimate        => popupQAEstimateWidget(popup)

    widget match
      case Some(popupWidget) => frame.renderWidget(popupWidget, at)
      case None              => ()

  private def popupEstimateWidget(popup: Popup, title: String): ParagraphWidget =
    val titleTxt = Spans.nostyle(title)
    val txt = popup.input
    val border = BlockWidget(title = Some(titleTxt), titleAlignment = Alignment.Center, borders = Borders.ALL)
    val paragraph = Array(Spans.from(Span.nostyle("Enter competency estimation:")), Spans.nostyle(txt))
    ParagraphWidget(text = Text(paragraph), block = Some(border), alignment = Alignment.Center)

  private def popupCompetencyEstimateWidget(popup: Popup) = popupEstimateWidget(popup, "Competencies estimate")

  private def popupQAEstimateWidget(popup: Popup) = popupEstimateWidget(popup, "QA estimate")

  private def popupRect(window: Rect): Rect =
    val (centerx, centery) = (window.width / 2, window.height / 2)
    val (xsize, ysize) = (40, 10)
    Rect(
      x = centerx - xsize / 2,
      y = centery - ysize / 2,
      width = xsize,
      height = ysize
    )

  private def handledCompetencyEstimationInput(key: KeyCode) =
    handleEstimationPopupInput(key, Focus.Competencies, submitCompetencyEstimate)

  private def submitCompetencyEstimate(input: String): CompetenciesView =
    val estimation = KnowledgeCompleteness.Answered(input.toInt)
    cntrl.estimatedCompetency(state.selected.competency, estimation)
    focusChanged(Focus.Competencies)

  private def handleEstimationPopupInput(
      key: KeyCode,
      focusOnClose: Focus,
      onInputSubmit: String => CompetenciesView
  ): CompetenciesView =
    val focus = state.focused.asInstanceOf[Focus.Popup]
    key match

      // Abort
      case _: KeyCode.Esc => focusChanged(focusOnClose)

      // Input symbols
      case symb: KeyCode.Char =>
        onlyDigitsValidator(String.valueOf(symb.c)) match
          // Incorrect input. TODO render error message
          case Left(err) => focusChanged(focusOnClose)
          // OK, add symbol to input
          case Right(_) => focusChanged(Focus.Popup(focus.kind, focus.input + symb.c))

      // Delete one input symbol
      case _: KeyCode.Backspace => focusChanged(Focus.Popup(focus.kind, focus.input.init))

      // Submit input
      case _: KeyCode.Enter =>
        numRangeValidator(0, 100)(focus.input) match
          // Incorrect input. TODO render error message
          case Left(err) => focusChanged(focusOnClose)
          // OK, estimate competency
          case Right(input) => onInputSubmit(input)

      case _ => this
  end handleEstimationPopupInput

  private def handledQAsEstimationInput(key: KeyCode) = handleEstimationPopupInput(key, Focus.QAs, submitQaEstimate)

  private def submitQaEstimate(input: String): CompetenciesView = state.selected.qaIndex
    .map: qa =>
      val estimation = KnowledgeCompleteness.Answered(input.toInt)
      cntrl.estimatedQA(state.selected.competency, qa, estimation)
      focusChanged(Focus.QAs)
    .getOrElse(this)

  private def currentCompetency: CompetencyView =
    cntrl.competencies.find(c => c.numeration == state.selected.competency).get
end CompetenciesView

object CompetenciesView:

  private val SelectedItemStyle: Style = Style(addModifier = Modifier.BOLD, bg = Some(Color.White))

  def competencyHeaderText(c: CompetencyView, knowledges: Map[Numeration, KnowledgeComputed]): String =
    val knowledgePart = knowledges get c.numeration match
      case None => "(?)"
      case Some(knowledge) =>
        knowledge.overridenBy match
          // Not overriden but synthetic, show percent with inherited mark
          case None if knowledge.synthetic => s"(${knowledge.percent}) →"
          // Not overriden, just show percent
          case None => s"(${knowledge.percent})"
          // Overriden, show percent with overriden mark
          case Some(_) => s"(← ${knowledge.percent})"

    s"${c.numerationView} : ${c.name} $knowledgePart"
  end competencyHeaderText

  def competenciesAtLevel(all: Seq[CompetencyView], level: Int): Seq[CompetencyView] = all
    .filter(c => c.numeration.size == level + 1)
    .sortBy(c => c.numeration.lift(level).getOrElse(0))

  def apply(cntrl: CompetenciesController)(using msgShow: MessageShow): CompetenciesView =
    val competencies = cntrl.competencies
    require(competencies.nonEmpty)
    val first = competencies.sortBy(_.numeration)(using NumerationOrdering).head
    val state = ViewState(selected = Selection(competency = first.numeration), focused = Focus.Competencies)

    val maxNestLevel = competencies.map(c => 0.max(c.numeration.size - 1)).max
    new CompetenciesView(cntrl, state, computeLayout(competencies, maxNestLevel), maxNestLevel, msgShow)

  def apply(state: ViewState)(using
      cntrl: CompetenciesController,
      msgShow: MessageShow
  ): CompetenciesView =
    val competencies = cntrl.competencies
    require(competencies.nonEmpty)
    val maxNestLevel = competencies.map(c => 0.max(c.numeration.size - 1)).max
    new CompetenciesView(cntrl, state, computeLayout(competencies, maxNestLevel), maxNestLevel, msgShow)

  def computeLayout(competencies: Seq[CompetencyView], maxNestLevel: Int): Layout =
    require(maxNestLevel >= 0)

    val estimateMargin = 7
    val gap = 2
    val fallbackMinW = 40 + gap

    val competenciesConstraints: Array[Constraint] = Array.from(
      for lvl <- 0 to maxNestLevel yield
        val headerMinW = competenciesAtLevel(competencies, lvl)
          .map(c => competencyHeaderText(c, Map.empty))
          .map(_.size)
          .maxOption
          .getOrElse(fallbackMinW)
        Constraint.Min(headerMinW + gap + estimateMargin)
    )
    Layout(Horizontal, constraints = competenciesConstraints)
  end computeLayout

  case class ViewState(
      // Current selected items
      val selected: Selection,
      // Current user input focus
      val focused: Focus,
      // Stack of previously selected elements
      val selectionContext: List[Selection] = List.empty
  ):
    def nestLevel: Int = selected.competency.size - 1

    def childSelected(child: Numeration): ViewState =
      copy(selected = Selection(competency = child), selectionContext = selected :: selectionContext)

    def parentSelected: ViewState =
      selectionContext match
        case newCurrent :: rest => copy(selected = newCurrent, selectionContext = rest)
        case Nil                => this

    def neighbourSelected(current: Numeration) = copy(selected = Selection(competency = current))

    def findSelectedAtLevel(level: Int): Option[Numeration] = selectionContext
      .find(s => s.competency.size == level + 1)
      .map(_.competency)

    def focusedOn(f: Focus): ViewState = copy(focused = f)

    def qaSelected(idx: Int): ViewState = copy(selected = selected.copy(qaIndex = Some(idx)))
  end ViewState

  case class Selection(
      val competency: Numeration,
      val qaIndex: Option[Int] = None
  )

  enum Focus derives CanEqual:
    case Competencies
    case QAs
    case Popup(kind: PopupType, input: String = "")

  enum PopupType derives CanEqual:
    case CompetencyEstimate
    case QAsEstimate
