package view

import tui.Alignment
import tui.Frame
import tui.Rect
import tui.Span
import tui.Spans
import tui.Text
import tui.crossterm.KeyCode
import tui.widgets.BlockWidget
import tui.widgets.ParagraphWidget
import tui.widgets.ParagraphWidget.Wrap

class LogView(
    var logs: Seq[String] = Vector.empty
) extends TuiView:
  override def handledKeyboard(key: KeyCode): TuiView = this

  override def render(frame: Frame, at: Rect): Unit =
    // Drop outdated logs
    logs = if logs.size > at.height then logs.takeRight(at.height) else logs

    // TODO clean logs after certain time period
    // TODO maybe introduce tick method to TuiView?
    val logsTxt = logs.map(log => Spans.from(Span.nostyle(log)))
    val widget = ParagraphWidget(text = Text(logsTxt.toArray), alignment = Alignment.Center)
    frame.renderWidget(widget, at)

  def updateLogs(msg: String): Unit = logs = logs appended msg

type MessageShow = (String) => Unit
