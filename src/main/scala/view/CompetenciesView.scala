package view

import com.typesafe.scalalogging.Logger
import common.*
import controller.CompetenciesController
import tui.*
import tui.Direction.Horizontal
import tui.Direction.Vertical
import tui.crossterm.KeyCode
import tui.widgets.BlockWidget
import tui.widgets.BlockWidget.BorderType
import tui.widgets.ListWidget
import tui.widgets.ParagraphWidget
import tui.widgets.ParagraphWidget.Wrap
import tui.widgets.canvas.CanvasWidget
import tui.widgets.canvas.Label
import view.CompetenciesView.Focus
import view.CompetenciesView.Focus.Popup
import view.CompetenciesView.PopupType
import view.CompetenciesView.ViewState
import tui.widgets.ClearWidget

val log: Logger = Logger(classOf[CompetenciesView])

private val SelectedItemStyle: Style = Style(bg = Some(Color.White))
private val CrossedItemStyle: Style = Style(addModifier = Modifier.CROSSED_OUT)
private val FootlineItemStyle: Style = Style(bg = Some(Color.Blue))

private val EmptySpans = Spans.nostyle("")

class CompetenciesView private (
    val cntrl: CompetenciesController,
    val state: ViewState,
    val competenciesLayout: Layout,
    val maxNestLevel: Int,
    val show: MessageShow
) extends TuiView:

  override def handledKeyboard(key: KeyCode): Option[CompetenciesView] = Some(state.focused match
    case Focus.Competencies => handledCompetenciesInput(key)
    case Focus.QAs          => handledQAsInput(key)
    case Popup(kind, _) =>
      kind match
        case PopupType.CompetencyEstimate => handledCompetencyEstimationInput(key)
        case PopupType.QAsEstimate        => handledQAsEstimationInput(key)
        case PopupType.CompetencyInsert   => handledCompetencyCreateInput(key, Some(state.selected.competency))
        case PopupType.CompetencyCreate   => handledCompetencyCreateInput(key, state.selected.competency.directParent)
        case PopupType.QACreate           => handledQACreateInput(key)
        case PopupType.QAShowAnswer       => handledQAAnswerInfoInput(key)
  )

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

      // Open popup input for competency creation, just change focus to create input dialog
      case f: KeyCode.F if f.num == 1 => focusChanged(Focus.Popup(PopupType.CompetencyCreate)) // As brother
      case f: KeyCode.F if f.num == 2 => focusChanged(Focus.Popup(PopupType.CompetencyInsert)) // As child

      // Open popup input for QA creation, just change focus
      case f: KeyCode.F if f.num == 3 => focusChanged(Focus.Popup(PopupType.QACreate))

      // Tab key changes focus
      case _: KeyCode.Tab => focusChanged(Focus.QAs)

      case _ => this
    end match
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
  end focusChanged

  private def changeFocusBetweenQACompetency(focus: Focus): CompetenciesView =
    // Init selected qa with first item in current competency
    val selectedQAIndex = (currentCompetency, state.selected.qaIndex) match
      case (competency, None) => Option.when(competency.questions.nonEmpty)(0)
      case (_, Some(sel))     => Some(sel)

    selectedQAIndex match
      case None             => this // Do not change focus - there are no qa items
      case Some(selectedQA) => withState(state.focusedOn(focus).qaSelected(selectedQA))
  end changeFocusBetweenQACompetency

  private def handledQAsInput(key: KeyCode): CompetenciesView =
    key match
      // Arrows
      case _: KeyCode.Up   => prevQASelected.getOrElse(this)
      case _: KeyCode.Down => nextQASelected.getOrElse(this)

      // Open popup input for QA estimation, just change focus
      case _: KeyCode.Enter => focusChanged(Focus.Popup(PopupType.QAsEstimate))

      // Erase qa estimate if selected
      case _: KeyCode.Delete if state.selected.qaIndex.isDefined =>
        cntrl.estimatedQA(state.selected.competency, state.selected.qaIndex.get, KnowledgeCompleteness.NotMentioned)
        this

      // Open Popup input for QA creation, just change focus
      case f: KeyCode.F if f.num == 1 => focusChanged(Focus.Popup(PopupType.QACreate))

      // Open Popup to show answer paragraph, just change focus
      case f: KeyCode.F if f.num == 2 => focusChanged(Focus.Popup(PopupType.QAShowAnswer))

      // Tab key changes focus
      case _: KeyCode.Tab => withState(state.focusedOn(Focus.Competencies))

      case _ => this
    end match
  end handledQAsInput

  private def nextQASelected: Option[CompetenciesView] =
    val qa: Seq[QA] = currentCompetency.questions
    val nextIdx: Option[Int] = state.selected.qaIndex
      .map(currIdx => (qa.size - 1).min(currIdx + 1))
      .orElse(Option.when(qa.nonEmpty)(0)) // or first idx if exists

    nextIdx.map(newIdx => state.qaSelected(newIdx)).map(withState(_))
  end nextQASelected

  private def prevQASelected: Option[CompetenciesView] =
    val prevIdx: Option[Int] = state.selected.qaIndex
      .map(currIdx => 0.max(currIdx - 1))
      .orElse(Option.when(currentCompetency.questions.nonEmpty)(0)) // or first idx if exists

    prevIdx.map(newIdx => state.qaSelected(newIdx)).map(withState(_))
  end prevQASelected

  override def render(frame: Frame, at: Rect) =
    val verticalChunks =
      Layout(Vertical, constraints = Array(Constraint.Percentage(99), Constraint.Percentage(1))).split(at)
    require(verticalChunks.size == 2)
    val (workingArea, footlineArea) = (verticalChunks(0), verticalChunks(1))
    val workingAreaChunks =
      Layout(Horizontal, constraints = Array(Constraint.Percentage(60), Constraint.Percentage(40))).split(workingArea)
    require(workingAreaChunks.size == 2)
    val (competencyArea, qaArea) = (workingAreaChunks(0), workingAreaChunks(1))
    renderCompetencies(frame, competencyArea)
    renderQA(frame, qaArea)
    renderPopup(frame, popupRect(at))
    renderFootline(frame, footlineArea)
  end render

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
  end renderCompetencies

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
  end renderCompetencyLevel

  private def competencyListItem(
      c: CompetencyView,
      nestLevel: Int,
      knowledges: Map[Numeration, KnowledgeComputed],
      at: Rect
  ): ListWidget.Item =
    val selectedStyle =
      if state.focused == Focus.Competencies && state.selected.competency == c.numeration then SelectedItemStyle
      else Style.DEFAULT
    val header = CompetenciesView
      .competencyHeader(c, knowledges)
      .withPatchedStyle(selectedStyle)
      .bounded(at)
    return ListWidget.Item(content = header)

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
    end if
  end toRenderCompetencies

  private def renderQA(frame: Frame, at: Rect) =
    val allQA = currentCompetency.questions
    val knowledges = cntrl.computedKnowledges
    val qaItems = (0 until allQA.size)
      .map(qaIdx => qaListItem(currentCompetency, qaIdx, knowledges, at))
      .toArray
    val widgetTitle = Some(Spans.nostyle("QAs"))
    val widgetBlock = Some(BlockWidget(borders = Borders.ALL, borderType = BorderType.Rounded, title = widgetTitle))
    val widget = ListWidget(items = qaItems, block = widgetBlock)
    frame.renderWidget(widget, at)
  end renderQA

  private def qaListItem(
      c: CompetencyView,
      qaIdx: Int,
      knowledges: Map[Numeration, KnowledgeComputed],
      at: Rect
  ): ListWidget.Item =
    // TODO add button to see answer only if it exists
    val selectedStyle =
      if state.focused == Focus.QAs && state.selected.qaIndex.getOrElse(-1) == qaIdx then SelectedItemStyle
      else Style.DEFAULT
    val header = qaHeader(c, qaIdx, knowledges)
      .withPatchedStyle(selectedStyle)
      .bounded(at)
    return ListWidget.Item(content = header)
  end qaListItem

  private def renderPopup(frame: Frame, at: Rect) =
    val activePopup: Option[Popup] = state.focused match
      case popup @ Popup(_, _) => Some(popup)
      case _                   => None

    val widget: Option[Widget] = activePopup.map: popup =>
      popup.kind match
        case PopupType.CompetencyEstimate => popupCompetencyEstimateWidget(popup)
        case PopupType.QAsEstimate        => popupQAEstimateWidget(popup)
        case PopupType.CompetencyCreate   => popupCompetencyCreateWidget(popup)
        case PopupType.CompetencyInsert   => popupCompetencyCreateWidget(popup)
        case PopupType.QACreate           => popupQACreateWidget(popup)
        case PopupType.QAShowAnswer       => qaAnswerWidget(at)

    widget.foreach: popupWidget =>
      frame.renderWidget(ClearWidget, at)
      frame.renderWidget(popupWidget, at)
  end renderPopup

  private def renderFootline(frame: Frame, at: Rect) =
    val canvas = CanvasWidget(xBounds = Point(0, at.width), yBounds = Point(0, at.height)): ctx =>
      ctx.labels ++= footlineLabels(at)
    frame.renderWidget(canvas, at)

  /** Returns sequence of (key, operationDescription) */
  private def footlineElems: Seq[(String, String)] = state.focused match
    // TODO Show by condition. For example, erase estimate must be shown only if estimate exists
    case Focus.Competencies =>
      Vector(
        ("ENTER", "Estimate"),
        ("DEL", "Erase estimate"),
        ("F1", "Create competency"),
        ("F2", "Create competency at child"),
        ("F3", "Create QA"),
        ("TAB", "Switch")
      )
    case Focus.QAs =>
      Vector(
        ("ENTER", "Estimate"),
        ("DEL", "Erase estimate"),
        ("F1", "Create QA"),
        ("F2", "Show answer"),
        ("TAB", "Switch")
      )
    case Popup(kind, _) =>
      kind match
        case _ => Vector(("ESC", "Abort"), ("ENTER", "Submit"))
  end footlineElems

  private def footlineLabels(at: Rect): Seq[Label] =
    val elems = footlineElems
    val (borderGap, itemsGap) = (5, 5)
    val spanByIdx = (i: Int) => Spans.styled(s"[${elems(i)._1}] ${elems(i)._2}", FootlineItemStyle)
    var lastPos = borderGap
    return (0 until elems.size).map: i =>
      if i == 0 then Label(x = borderGap, y = 0d, spans = spanByIdx(i))
      else
        val pos = lastPos + itemsGap + spanByIdx(i - 1).width
        lastPos = pos
        Label(x = pos, y = 0d, spans = spanByIdx(i))
  end footlineLabels

  private def popupInputWidget(popup: Popup, title: String, prompt: String): ParagraphWidget =
    val titleTxt = Spans.nostyle(title)
    val txt = popup.input
    val border = BlockWidget(title = Some(titleTxt), titleAlignment = Alignment.Center, borders = Borders.ALL)
    val paragraph = Array(Spans.from(Span.nostyle(prompt)), Spans.nostyle(txt))
    ParagraphWidget(text = Text(paragraph), block = Some(border), alignment = Alignment.Center)

  private def popupCompetencyEstimateWidget(popup: Popup) =
    popupInputWidget(popup, "Estimate competency", "Enter competency estimation:")

  private def popupCompetencyCreateWidget(popup: Popup) =
    popupInputWidget(popup, "Create competency", "Enter competency name:")

  private def popupQAEstimateWidget(popup: Popup) = popupInputWidget(popup, "Estimate QA", "Enter QA estimation:")

  private def popupQACreateWidget(p: Popup) = popupInputWidget(p, "Create QA", "Enter question:")

  private def popupRect(window: Rect): Rect =
    val (centerx, centery) = (window.width / 2, window.height / 2)
    val (xsize, ysize) = (60, 15)
    Rect(
      x = centerx - xsize / 2,
      y = centery - ysize / 2,
      width = xsize,
      height = ysize
    )

  private def qaAnswerWidget(at: Rect) = popupInfoWidget(
    at,
    "Answer",
    state.selected.qaIndex.flatMap(qa => currentCompetency.questions(qa).answerBody).getOrElse("")
  )

  private def popupInfoWidget(at: Rect, title: String, info: String): ParagraphWidget =
    val titleTxt = Spans.nostyle(title)
    val border = BlockWidget(
      title = Some(titleTxt),
      titleAlignment = Alignment.Center,
      borders = Borders.ALL,
      borderType = BorderType.Thick
    )
    val paragraph = Array(Spans.from(Span.nostyle(info)))
    return ParagraphWidget(
      text = Text(paragraph),
      block = Some(border),
      alignment = Alignment.Left,
      wrap = Some(Wrap(true))
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
    end match
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

  def qaHeader(at: CompetencyView, qaIdx: Int, knowledges: Map[Numeration, KnowledgeComputed]): Spans =
    val qa = at.questions(qaIdx)
    val estimatedPercent = qaKnowledgePercent(qa.status)
    val knowledgePart = knowledges get at.numeration match
      case None => EmptySpans
      case Some(knowledge) =>
        knowledge.overridenBy match
          case None if estimatedPercent.isEmpty    => EmptySpans
          case None                                => Spans.nostyle(s"(${estimatedPercent.get})")
          case Some(_) if estimatedPercent.isEmpty => EmptySpans
          case Some(overrides)                     => Spans.styled(s"(${estimatedPercent.get})", CrossedItemStyle)
    return Spans(knowledgePart.spans prepended Span.nostyle(s"${qaIdx + 1}: ${qa.questionBody}"))
  end qaHeader

  private def qaKnowledgePercent(kc: KnowledgeCompleteness): Option[Int] = kc match
    case KnowledgeCompleteness.NotMentioned      => None
    case KnowledgeCompleteness.Answered(percent) => Some(percent)
    case KnowledgeCompleteness.Unanswered        => Some(0)

  private def handledCompetencyCreateInput(key: KeyCode, parentOfCreated: Option[Numeration]): CompetenciesView =
    val focus = state.focused.asInstanceOf[Focus.Popup]
    key match
      // Abort
      case _: KeyCode.Esc => focusChanged(Focus.Competencies)

      // Create competency
      case _: KeyCode.Enter =>
        cntrl.createCompetency(parentOfCreated, focus.input)
        focusChanged(Focus.Competencies)

      // Input symbols
      case symb: KeyCode.Char =>
        notEmptyValidator(String.valueOf(symb.c)) match
          // Incorrect input. TODO render error message
          case Left(err) => focusChanged(Focus.Competencies)
          // OK, add symbol to input
          case Right(gotInput) => focusChanged(Focus.Popup(focus.kind, focus.input + gotInput))

      // Delete one input symbol
      case _: KeyCode.Backspace => focusChanged(Focus.Popup(focus.kind, focus.input.init))

      case _ => this
    end match
  end handledCompetencyCreateInput

  private def handledQACreateInput(key: KeyCode): CompetenciesView =
    val focus = state.focused.asInstanceOf[Focus.Popup]
    key match
      // Abort
      case _: KeyCode.Esc => focusChanged(Focus.QAs)

      // Create QA
      case _: KeyCode.Enter =>
        notEmptyValidator(focus.input) match
          // Incorrect input. TODO render error message
          case Left(err) => focusChanged(Focus.QAs)
          // OK, create QA
          case Right(question) =>
            cntrl.createQA(state.selected.competency, question)
            if state.selected.qaIndex.isDefined then focusChanged(Focus.QAs) else focusChanged(Focus.Competencies)

      // Input symbols
      case symb: KeyCode.Char =>
        notEmptyValidator(String.valueOf(symb.c)) match
          // Incorrect input. TODO render error message
          case Left(err) => focusChanged(Focus.QAs)
          // OK, add symbol to input
          case Right(gotInput) => focusChanged(Focus.Popup(focus.kind, focus.input + gotInput))

      // Delete one input symbol
      case _: KeyCode.Backspace => focusChanged(Focus.Popup(focus.kind, focus.input.init))

      case _ => this
    end match
  end handledQACreateInput

  private def handledQAAnswerInfoInput(key: KeyCode) = handledInfoWidgetInput(key, Focus.QAs)

  private def handledInfoWidgetInput(key: KeyCode, focusOnClose: Focus): CompetenciesView = key match
    case _: (KeyCode.Esc | KeyCode.Enter) => focusChanged(focusOnClose)
    case _                                => this
end CompetenciesView

object CompetenciesView:
  def competencyHeader(c: CompetencyView, knowledges: Map[Numeration, KnowledgeComputed]): Spans =
    val knowledgePart = knowledges get c.numeration match
      case None => EmptySpans
      case Some(knowledge) =>
        knowledge.overridenBy match
          // Not overriden but synthetic, show percent with inherited mark
          case None if knowledge.synthetic => Spans.nostyle(s"(${knowledge.percent}) →")
          // Not overridden, just show percent
          case None => Spans.nostyle(s"(${knowledge.percent})")
          // Overridden, show percent with overriden mark
          case Some(overrides) =>
            val overridesPercent = Span.nostyle(s"(← ${(knowledges get overrides).get.percent})")
            val percentFromInput = Span.styled(s"(${knowledge.percent})", CrossedItemStyle)
            Spans.from(percentFromInput, overridesPercent)
    return Spans(knowledgePart.spans prepended Span.nostyle(s"${c.numerationView}: ${c.name}"))
  end competencyHeader

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

    val estimateMargin = 12
    val gap = 2
    val fallbackMinW = 40 + gap

    val competenciesConstraints: Array[Constraint] = Array.from(
      for lvl <- 0 to maxNestLevel yield
        val headerMinW = competenciesAtLevel(competencies, lvl)
          .map(c => competencyHeader(c, Map.empty))
          .map(_.width)
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
    // Estimate competency
    case CompetencyEstimate
    // Estimate QA
    case QAsEstimate
    // Create competency at current nest level
    case CompetencyCreate
    // Create competency and insert it to child of current selected competency
    case CompetencyInsert
    // Create QA
    case QACreate
    // Show answer
    case QAShowAnswer
