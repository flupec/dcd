package view

import com.typesafe.scalalogging.Logger
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

val log: Logger = Logger(classOf[NumeratedListView])

class NumeratedListView private (
    val competencies: Seq[CompetencyView],
    val state: ViewState,
    val competenciesLayout: Layout,
    val maxNestLevel: Int
) extends Renderable:

  def childSelected: Option[NumeratedListView] =
    val childs = childCompetencies
    if childs.isEmpty then None
    else childs.headOption.map(select => withState(state.childSelected(select.numeration)))

  private def childCompetencies: Seq[CompetencyView] = competencies
    .filter(c => c.numeration.directParent.map(p => p == state.selectedCompetency).getOrElse(false))

  def parentSelected: Option[NumeratedListView] =
    if state.selectionContext.nonEmpty then Some(withState(state.parentSelected)) else None

  def nextSelected: Option[NumeratedListView] = competencies
    .find(c => c.numeration == state.selectedCompetency.next)
    .map(next => withState(state.competencySelected(next.numeration)))

  def prevSelected: Option[NumeratedListView] = competencies
    .find(c => c.numeration == state.selectedCompetency.previous)
    .map(next => withState(state.competencySelected(next.numeration)))

  private def withState(s: ViewState) = NumeratedListView(competencies, s)

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
    val selectedStyle = if state.selectedCompetency == c.numeration then SelectedItemStyle else Style.DEFAULT
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
      atLevel.filter(c => c.numeration.isChildOf(state.selectedCompetency))
    else if level == 0 then
      // Render all cuz items at root should be shown as-is
      atLevel
    else
      // Render only direct childs of previous level selected items
      val prevLevelSelected = state.findSelectedAtLevel(level - 1).get
      atLevel.filter(c => c.numeration.isChildOf(prevLevelSelected))

  private def renderQA(frame: Frame, at: Rect) =
    val widget = ListWidget(items = currentCompetency.questions.map(qa => qaListItem(qa, at)).toArray)
    frame.renderWidget(widget, at)

  private def qaListItem(qa: QA, at: Rect): ListWidget.Item =
    // val selectedStyle = if state.currentSelected == c.numeration then SelectedItemStyle else Style.DEFAULT
    // TODO selected
    val selectedStyle = Style.DEFAULT
    val q = Some(Span.styled(s"Q: ${qa.questionBody}", selectedStyle).bounded(at))
    val a = qa.answerBody.map(ans => s"A: ${ans}").map(ans => Span.styled(ans, selectedStyle).bounded(at))
    val qaText = Array(q, a).filter(_.isDefined).map(_.get).reduceLeft((left, right) => left.concat(right))
    ListWidget.Item(content = qaText)
    
  private def currentCompetency: CompetencyView = competencies.find(c => c.numeration == state.selectedCompetency).get
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
    val state = ViewState(selectedCompetency = first.numeration)

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
      val selectedCompetency: Numeration,
      val selectionContext: List[Numeration] = List.empty // Stack of previously selected selectedCompetency numerations
  ):
    def nestLevel: Int = selectedCompetency.size - 1

    def childSelected(child: Numeration): ViewState =
      copy(selectedCompetency = child, selectionContext = selectedCompetency :: selectionContext)

    def parentSelected: ViewState =
      selectionContext match
        case newCurrent :: rest => copy(selectedCompetency = newCurrent, selectionContext = rest)
        case Nil                => this

    def competencySelected(current: Numeration) = copy(selectedCompetency = current)

    def findSelectedAtLevel(level: Int): Option[Numeration] = selectionContext.find(n => n.size == level + 1)

  end ViewState
