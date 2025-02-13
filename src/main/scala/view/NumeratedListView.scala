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

class NumeratedListView private (
    val competencies: Seq[CompetencyView],
    val state: ViewState,
    val layout: Layout,
    val log: Logger = Logger(classOf[NumeratedListView])
) extends Renderable:

  def childSelected: Option[NumeratedListView] =
    val childs = childCompetencies
    if childs.isEmpty then None
    else childs.headOption.map(select => withState(state.childSelected(select.numeration)))

  private def childCompetencies: Seq[CompetencyView] = competencies
    .filter(c => c.numeration.directParent.map(p => p == state.currentSelected).getOrElse(false))

  def parentSelected: Option[NumeratedListView] =
    if state.selectionContext.nonEmpty then Some(withState(state.parentSelected)) else None

  def nextSelected: Option[NumeratedListView] = competencies
    .find(c => c.numeration == state.currentSelected.next)
    .map(next => withState(state.currentSelected(next.numeration)))

  def prevSelected: Option[NumeratedListView] = competencies
    .find(c => c.numeration == state.currentSelected.previous)
    .map(next => withState(state.currentSelected(next.numeration)))

  private def withState(s: ViewState) = NumeratedListView(competencies, s)

  override def render(frame: Frame, at: Rect) =
    val chunks: Array[Rect] = layout.split(at)
    require(chunks.size == state.nestLevel + 1, s"Chunks size=${chunks.size}, nestLevel=${state.nestLevel}")
    for level <- 0 to state.nestLevel do renderLevel(frame, chunks(level), level)

  private def renderLevel(frame: Frame, at: Rect, nestLevel: Int) =
    val competenciesToRender: Array[ListWidget.Item] = toRenderCompetencies(nestLevel)
      .map(competencyListItem(_, nestLevel))
      .toArray

    val widget = ListWidget(items = competenciesToRender)
    frame.renderWidget(widget, at)

  private def competencyListItem(c: CompetencyView, nestLevel: Int): ListWidget.Item =
    val selectedStyle = if state.currentSelected == c.numeration then SelectedItemStyle else Style.DEFAULT

    val header = Text.from(Span.styled(c.numerationView + ": " + c.name, selectedStyle))
    ListWidget.Item(content = header)

  private def competenciesAtLevel(level: Int): Seq[CompetencyView] = competencies
    .filter(c => c.numeration.size == level + 1)
    .sortBy(c => c.numeration.lift(level).getOrElse(0))

  private def toRenderCompetencies(level: Int): Seq[CompetencyView] =
    val atLevel = competenciesAtLevel(level)
    if level == 0 || level < state.nestLevel then atLevel
    else
      val prev = state.prevSelection
      atLevel.filter(c => prev.map(p => c.numeration.isChildOf(p)).getOrElse(false))

end NumeratedListView

object NumeratedListView:

  private val SelectedItemStyle: Style = Style(addModifier = Modifier.BOLD, bg = Some(Color.White))

  def apply(competencies: Seq[CompetencyView]): NumeratedListView =
    require(competencies.nonEmpty)
    val first = competencies.sortBy(c => c.numeration.lift(0).getOrElse(0)).head
    val state = ViewState(currentSelected = first.numeration)

    apply(competencies, state)

  def apply(competencies: Seq[CompetencyView], state: ViewState): NumeratedListView =
    require(competencies.nonEmpty)
    new NumeratedListView(competencies, state, computeLayout(state))

  def computeLayout(state: ViewState): Layout =
    val nestLevel = state.nestLevel
    require(nestLevel >= 0)

    val occupyPercent = 100 / (nestLevel + 1)
    val occupyCorrection = 100 % (nestLevel + 1)

    val constraints: Array[Constraint] = Array.from(
      for lvl <- 0 to nestLevel
      yield Constraint.Percentage(occupyPercent + occupyCorrection)
    )
    Layout(Horizontal, constraints = constraints)

  case class ViewState(
      val currentSelected: Numeration,
      val selectionContext: List[Numeration] = List.empty // Stack of previously selected currentSelected numerations
  ):
    def nestLevel: Int = currentSelected.size - 1

    def childSelected(child: Numeration): ViewState =
      copy(currentSelected = child, selectionContext = currentSelected :: selectionContext)

    def parentSelected: ViewState =
      selectionContext match
        case newCurrent :: rest => copy(currentSelected = newCurrent, selectionContext = rest)
        case Nil                => this

    def currentSelected(current: Numeration) = copy(currentSelected = current)

    def prevSelection: Option[Numeration] = if selectionContext.nonEmpty then selectionContext.headOption else None

  end ViewState
