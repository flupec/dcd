package view

import tui.Frame
import tui.Rect

trait Renderable:

  /** Render this view to N horizontal regions
    *
    * @param frame render target
    * @param at view region, contains N horizontal regions
    */
  def render(frame: Frame, at: Rect): Unit
