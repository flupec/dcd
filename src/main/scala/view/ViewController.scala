package view

import tui.Frame
import tui.Rect
import tui.crossterm.CrosstermJni
import tui.crossterm.Event
import tui.crossterm.KeyCode

/** Controller for multiple views, handles input, focuses etc
  *
  * @param focusedOn focus view
  */
class ViewController(
    private var focusedOn: NumeratedListView
) extends Renderable:

  override def render(frame: Frame, at: Rect): Unit = focusedOn.render(frame, at)

  def handleInput(jni: CrosstermJni) =
    if jni.poll(tui.crossterm.Duration(0, 1000)) then
      jni.read() match
        case key: Event.Key => handleKeyboardInput(key.keyEvent().code())
        case _              => ()

  private def handleKeyboardInput(key: KeyCode): Unit =
    focusedOn match
      case _: NumeratedListView => handleInputForCompetenciesView(key)

  private def handleInputForCompetenciesView(key: KeyCode): Unit =
    val view = focusedOn.asInstanceOf[NumeratedListView]
    key match
      // Arrows
      case _: KeyCode.Up    => focusedOn = view.prevSelected.getOrElse(view)
      case _: KeyCode.Down  => focusedOn = view.nextSelected.getOrElse(view)
      case _: KeyCode.Left  => focusedOn = view.parentSelected.getOrElse(view)
      case _: KeyCode.Right => focusedOn = view.childSelected.getOrElse(view)

      // Exit
      case char: KeyCode.Char if char.c() == 'q' => System.exit(0)
      case _                                     => ()

object ViewController:
  def apply(view: NumeratedListView) = new ViewController(view)
