package view

import tui.Color
import tui.Constraint
import tui.Direction.Horizontal
import tui.Frame
import tui.Layout
import tui.Modifier
import tui.Rect
import tui.Span
import tui.Style
import tui.Text
import tui.widgets.ListWidget
import view.NumeratedListView.SelectedItemStyle
import view.NumeratedListView.ViewState
import tui.crossterm.KeyCode
import view.NumeratedListView.Focus

class NumeratedListView private (
    val competencies: Seq[CompetencyView],
    val state: ViewState,
    val competenciesLayout: Layout,
    val maxNestLevel: Int
) extends TuiView:

  override def handledKeyboard(key: KeyCode): NumeratedListView = state.focused match
    case Focus.Competencies => handledCompetenciesInput(key)
    case Focus.QAs          => handledQAsInput(key)

  private def handledCompetenciesInput(key: KeyCode): NumeratedListView =
    key match
      // Arrows
      case _: KeyCode.Up    => prevCompetencySelected.getOrElse(this)
      case _: KeyCode.Down  => nextCompetencySelected.getOrElse(this)
      case _: KeyCode.Left  => parentCompetencySelected.getOrElse(this)
      case _: KeyCode.Right => childCompetencySelected.getOrElse(this)

      // Tab key changes focus
      case _: KeyCode.Tab => focusChanged(Focus.QAs)

      // Exit
      case char: KeyCode.Char if char.c() == 'q' =>
        System.exit(0)
        this
      case _ => this

  private def childCompetencySelected: Option[NumeratedListView] =
    val childs = childCompetencies
    if childs.isEmpty then None
    else childs.headOption.map(select => withState(state.childSelected(select.numeration)))

  private def childCompetencies: Seq[CompetencyView] = competencies
    .filter(c => c.numeration.directParent.map(p => p == state.selected.competency).getOrElse(false))

  private def parentCompetencySelected: Option[NumeratedListView] =
    if state.selectionContext.nonEmpty then Some(withState(state.parentSelected)) else None

  private def nextCompetencySelected: Option[NumeratedListView] = competencies
    .find(c => c.numeration == state.selected.competency.next)
    .map(next => withState(state.neighbourSelected(next.numeration)))

  private def prevCompetencySelected: Option[NumeratedListView] = competencies
    .find(c => c.numeration == state.selected.competency.previous)
    .map(next => withState(state.neighbourSelected(next.numeration)))

  private def withState(s: ViewState) = NumeratedListView(competencies, s)

  private def focusChanged(focus: Focus): NumeratedListView =
    // Init selected qa with first item in current competency
    val selectedQAIndex = (currentCompetency, state.selected.qaIndex) match
      case (competency, None) => Option.when(competency.questions.nonEmpty)(0)
      case (_, Some(sel))     => Some(sel)

    selectedQAIndex match
      case None             => this // Do not change focus - there are no qa items
      case Some(selectedQA) => withState(state.focusedOn(focus).qaSelected(selectedQA))

  private def handledQAsInput(key: KeyCode): NumeratedListView =
    key match
      // Arrows
      case _: KeyCode.Up   => prevQASelected.getOrElse(this)
      case _: KeyCode.Down => nextQASelected.getOrElse(this)

      // Tab key changes focus
      case _: KeyCode.Tab => withState(state.focusedOn(Focus.Competencies))

      // Exit
      case char: KeyCode.Char if char.c() == 'q' =>
        System.exit(0)
        this
      case _ => this

  private def nextQASelected: Option[NumeratedListView] =
    val qa: Seq[QA] = currentCompetency.questions
    val nextIdx: Option[Int] = state.selected.qaIndex
      .map(currIdx => (qa.size - 1).min(currIdx + 1))
      .orElse(Option.when(qa.nonEmpty)(0)) // or first idx if exists

    nextIdx.map(newIdx => state.qaSelected(newIdx)).map(withState(_))

  private def prevQASelected: Option[NumeratedListView] =
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

  private def renderCompetencies(frame: Frame, at: Rect) =
    val chunks: Array[Rect] = competenciesLayout.split(at)
    require(chunks.size == maxNestLevel + 1, s"Chunks size=${chunks.size}, maxNestLevel=${state.nestLevel}")
    // TODO Ideally, we should render only 3 levels: previous, current and last, and use special symbols to show that
    // you can go lower / higher in the hierarchy. If we don't do this, then with a large nesting of competencies, the
    // render will be broken, all competencies will be cut off by the composer
    for level <- 0 to maxNestLevel do renderCompetencyLevel(frame, chunks(level), level)

  private def renderCompetencyLevel(frame: Frame, at: Rect, nestLevel: Int) =
    val competenciesToRender: Array[ListWidget.Item] = toRenderCompetencies(nestLevel)
      .map(competencyListItem(_, nestLevel, at))
      .toArray

    val widget = ListWidget(items = competenciesToRender)
    frame.renderWidget(widget, at)

  private def competencyListItem(c: CompetencyView, nestLevel: Int, at: Rect): ListWidget.Item =
    val selectedStyle =
      if state.focused == Focus.Competencies && state.selected.competency == c.numeration then SelectedItemStyle
      else Style.DEFAULT
    val header = Span.styled(NumeratedListView.competencyHeaderText(c), selectedStyle).bounded(at)
    ListWidget.Item(content = header)

  private def competenciesAtLevel(level: Int): Seq[CompetencyView] =
    NumeratedListView.competenciesAtLevel(competencies, level)

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
    val widget = ListWidget(items =
      (0 until allQA.size)
        .map(idx => allQA(idx) -> idx)
        .map((qa, qaIdx) => qaListItem(qa, qaIdx, at))
        .toArray
    )
    frame.renderWidget(widget, at)

  private def qaListItem(qa: QA, qaIdx: Int, at: Rect): ListWidget.Item =
    val selectedStyle =
      if state.focused == Focus.QAs && state.selected.qaIndex.getOrElse(-1) == qaIdx then SelectedItemStyle
      else Style.DEFAULT
    val q = Some(Span.styled(s"Q: ${qa.questionBody}", selectedStyle).bounded(at))
    val a = qa.answerBody.map(ans => s"A: ${ans}").map(ans => Span.styled(ans, selectedStyle).bounded(at))
    val qaText = Array(q, a).filter(_.isDefined).map(_.get).reduceLeft((left, right) => left.concat(right))
    ListWidget.Item(content = qaText)

  private def currentCompetency: CompetencyView = competencies.find(c => c.numeration == state.selected.competency).get
end NumeratedListView

object NumeratedListView:

  private val SelectedItemStyle: Style = Style(addModifier = Modifier.BOLD, bg = Some(Color.White))

  def competencyHeaderText(c: CompetencyView): String = c.numerationView + ": " + c.name

  def competenciesAtLevel(all: Seq[CompetencyView], level: Int): Seq[CompetencyView] = all
    .filter(c => c.numeration.size == level + 1)
    .sortBy(c => c.numeration.lift(level).getOrElse(0))

  def apply(competencies: Seq[CompetencyView]): NumeratedListView =
    require(competencies.nonEmpty)
    val first = competencies.sortBy(c => c.numeration.lift(0).getOrElse(0)).head
    val state = ViewState(selected = Selection(competency = first.numeration), focused = Focus.Competencies)

    apply(competencies, state)

  def apply(competencies: Seq[CompetencyView], state: ViewState): NumeratedListView =
    require(competencies.nonEmpty)
    val maxNestLevel = competencies.map(c => 0.max(c.numeration.size - 1)).max
    new NumeratedListView(competencies, state, computeLayout(competencies, maxNestLevel), maxNestLevel)

  def computeLayout(competencies: Seq[CompetencyView], maxNestLevel: Int): Layout =
    require(maxNestLevel >= 0)

    val gap = 2
    val fallbackMinW = 40 + gap

    val competenciesConstraints: Array[Constraint] = Array.from(
      for lvl <- 0 to maxNestLevel yield
        val headerMinW = competenciesAtLevel(competencies, lvl)
          .map(competencyHeaderText)
          .map(_.size)
          .maxOption
          .getOrElse(fallbackMinW)
        Constraint.Min(headerMinW + gap)
    )
    Layout(Horizontal, constraints = competenciesConstraints)

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

  enum Focus:
    case Competencies
    case QAs
