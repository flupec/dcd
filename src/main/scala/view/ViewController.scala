package view

import tui.Frame
import tui.Rect
import tui.crossterm.CrosstermJni
import tui.crossterm.Event
import tui.crossterm.KeyCode
import com.typesafe.scalalogging.Logger

/** Controller for multiple views, handles input, focuses etc
  *
  * @param main main view
  */
class ViewController(
    private var main: TuiView,
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

  override def render(frame: Frame, at: Rect): Unit = main.render(frame, at)

object ViewController:
  def apply(view: NumeratedListView) = new ViewController(view)
