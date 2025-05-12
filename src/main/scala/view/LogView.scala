package view

import tui.Alignment
import tui.Borders
import tui.Frame
import tui.Rect
import tui.Span
import tui.Spans
import tui.Text
import tui.crossterm.KeyCode
import tui.widgets.BlockWidget
import tui.widgets.BlockWidget.BorderType
import tui.widgets.ParagraphWidget

class LogView(
    var logs: Seq[String] = Vector.empty
) extends TuiView:
  override def handledKeyboard(key: KeyCode): Option[TuiView] = Some(this)

  override def render(frame: Frame, at: Rect): Unit =
    // Drop outdated logs
    logs = if logs.size > at.height then logs.takeRight(at.height) else logs

    // TODO clean logs after certain time period
    // TODO maybe introduce tick method to TuiView?
    val logsTxt = logs.map(log => Spans.from(Span.nostyle(log)))
    val widgetBlock =
      BlockWidget(
        title = Some(Spans.nostyle("Logs")),
        borders = Borders.ALL,
        borderType = BorderType.Rounded
      )
    val widget = ParagraphWidget(
      text = Text(logsTxt.toArray),
      alignment = Alignment.Center,
      block = Some(widgetBlock)
    )
    frame.renderWidget(widget, at)
  end render

  def updateLogs(msg: String): Unit = logs = logs appended msg

type MessageShow = (String) => Unit
