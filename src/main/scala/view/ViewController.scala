package view

import tui.Frame
import tui.Rect
import tui.crossterm.CrosstermJni
import tui.crossterm.Event
import tui.crossterm.KeyCode
import com.typesafe.scalalogging.Logger
import tui.Layout
import tui.Direction.Vertical
import tui.Constraint

/** Controller for multiple views, handles input, focuses etc
  *
  * @param main main view
  */
class ViewController(
    private var main: TuiView,
    private val logbar: LogView,
    val log: Logger = Logger(classOf[ViewController])
) extends TuiView:

  def handleInput(jni: CrosstermJni) =
    if jni.poll(tui.crossterm.Duration(0, 1000)) then
      jni.read() match
        case key: Event.Key => handledKeyboard(key.keyEvent().code())
        case _              => ()

  override def handledKeyboard(key: KeyCode): TuiView =
    main = main.handledKeyboard(key)
    this

  override def render(frame: Frame, at: Rect): Unit =
    val layout = Layout(
      direction = Vertical,
      constraints = Array(Constraint.Percentage(90), Constraint.Percentage(10))
    )
    val chunks = layout.split(at)
    main.render(frame, chunks(0))
    logbar.render(frame, chunks(1))

object ViewController:
  def apply(view: CompetenciesView, logbar: LogView) = new ViewController(view, logbar)
