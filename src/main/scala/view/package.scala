package view

import tui.Frame
import tui.Rect
import tui.crossterm.KeyCode

trait TuiView:
  /** Returns copy with updated state due keyboard key pressed.
   *  Returns None if tui view not exist anymore, for example when main view needs to shutdown gracefully
    *
    * @param key key code of pressed key
    * @return new state due user input
    */
  def handledKeyboard(key: KeyCode): Option[TuiView]

  /** Render this view to N horizontal regions
    *
    * @param frame render target
    * @param at view region, contains N horizontal regions
    */
  def render(frame: Frame, at: Rect): Unit
