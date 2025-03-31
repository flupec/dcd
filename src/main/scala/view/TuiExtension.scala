package view

import tui.Rect
import tui.Span
import tui.Spans
import tui.Style
import tui.Text

extension (span: Span) infix def concat(another: Span) = span.copy(content = span.content.concat(another.content))

extension (spans: Spans)
  /** Wrap these spans to meet bound criteria */
  def bounded(bound: Rect): Text = toText(bound.width)

  /** Wrap these spans to text (multiline spans) if content exceeds maxW limit */
  def toText(maxW: Int): Text =
    val lines = scala.collection.mutable.ListBuffer.empty[Spans]
    var (spanIdx, col, wrapTgt) = (0, 0, spans.copy().spans)

    while spanIdx < wrapTgt.length do
      val currSpan = wrapTgt(spanIdx)
      val lineFits = maxW - col
      if currSpan.width <= lineFits then
        lines.lastOption match
          case None                => lines.addOne(Spans.from(currSpan))
          case Some(_) if col == 0 => lines.addOne(Spans.from(currSpan))
          case Some(lineSpans)     => lines.update(lines.size - 1, lineSpans concat Spans.from(currSpan))
        col = col + currSpan.width // Stays at same line
        spanIdx = spanIdx + 1
      else
        val (fits, rest) = currSpan.content.splitAt(lineFits)
        val fitsSpan = Span.styled(fits, currSpan.style)
        val restSpan = Span.styled(rest, currSpan.style)
        lines.lastOption match
          case None if col == 0 => lines.addOne(Spans.from(fitsSpan))
          case None             => lines.update(lines.size - 1, Spans.from(fitsSpan))
          case Some(value)      => lines.update(lines.size - 1, Spans(value.spans appended fitsSpan))
        wrapTgt(spanIdx) = restSpan
        col = 0 // New line
    return Text(lines.toArray)
  end toText

  infix def concat(another: Spans) = spans.copy(spans = spans.spans concat another.spans)

  def withPatchedStyle(style: Style) = Spans(spans.spans.map(s => s.copy(style = s.style.patch(style))))

extension (text: Text) infix def concat(another: Text): Text = Text(text.lines appendedAll another.lines)
