package view

import tui.Span
import tui.Text
import tui.Spans
import tui.Rect

extension (span: Span)
  /** Wrap this span to meet bound criteria */
  def bounded(bound: Rect): Text = toText(bound.width)

  /** Wrap this span to text (multiline span) if content exceeds maxW limit */
  def toText(maxW: Int): Text =
    val fullLines = span.content.size / maxW
    val halfLines = if span.content.size % maxW > 0 then 1 else 0

    val splitted = for i <- 0 until fullLines + halfLines yield
      val src = span.content
      val from = i * maxW
      val to = src.size.min((i + 1) * maxW)
      Span.styled(src.substring(from, to), span.style)

    Text(splitted.map(s => Spans.from(s)).toArray)

extension (text: Text) def concat(another: Text): Text = Text(text.lines appendedAll another.lines)
