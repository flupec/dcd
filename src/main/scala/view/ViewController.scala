package view

import com.typesafe.scalalogging.Logger
import tui.Constraint
import tui.Direction.Vertical
import tui.Frame
import tui.Layout
import tui.Rect
import tui.crossterm.CrosstermJni
import tui.crossterm.Event
import tui.crossterm.KeyCode

/** Controller for multiple views, handles input, focuses etc
  *
  * @param main main view
  */
class ViewController(
    private var main: TuiView,
    private val logbar: LogView,
    val log: Logger = Logger(classOf[ViewController])
) extends TuiView:

  /* Returns true if session must continue. Returns false if user want to exit from application */
  def handleInput(jni: CrosstermJni): Boolean =
    val hasNewEvents = jni.poll(tui.crossterm.Duration(0, 250))
    if !hasNewEvents then return true

    jni.read match
      case quit: Event.Key if isQuitEvent(quit) => false
      case key: Event.Key                       => handledKeyboard(key.keyEvent.code).isDefined
      case _                                    => true
  end handleInput

  private def isQuitEvent(e: Event.Key): Boolean =
    e.keyEvent.code match
      case c: KeyCode.Char => c.c == 'q'
      case _               => false

  override def handledKeyboard(key: KeyCode): Option[TuiView] =
    main.handledKeyboard(key) match
      case None => None
      case Some(updatedMain) =>
        main = updatedMain
        Some(this)

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
