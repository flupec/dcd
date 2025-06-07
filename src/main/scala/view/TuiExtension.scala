package view

import tui.Rect
import tui.Span
import tui.Spans
import tui.Style
import tui.Text

extension (span: Span) infix def concat(another: Span) = span.copy(content = span.content.concat(another.content))

extension (spans: Spans)
  /** Trim these spans to meet bound criteria */
  def trimmed(bound: Rect): Spans = trimmed(bound.width)

  def trimmed(maxW: Int): Spans =
    require(maxW >= 1)
    val maxWidth = maxW - 2 // Width + left bound char + right bound char
    var width = 0
    val trimmed =
      for s <- spans.spans
      yield
        if width + s.width < maxWidth then
          width += s.width
          s
        else
          val fits = maxWidth - width
          width += fits
          val fitsContent = s.content.substring(0, fits)
          val withDots = fitsContent.length match
            case n if n <= 3 => ".".repeat(3)
            case n           => fitsContent.substring(0, n - 3).concat(".".repeat(3))
          Span(content = withDots, style = s.style)
    return Spans(trimmed)

  infix def concat(another: Spans) = spans.copy(spans = spans.spans concat another.spans)

  def withPatchedStyle(style: Style) = Spans(spans.spans.map(s => s.copy(style = s.style.patch(style))))

extension (text: Text)
  infix def concat(another: Text): Text = Text(text.lines appendedAll another.lines)

  /** Returns text with new lines added if any of span in line contains EOL */
  def toMultipleLines: Text =
    val containsEOL = (s: Spans) => s.toString.contains('\n')
    val lines = scala.collection.mutable.ListBuffer.empty[Spans]

    for line <- text.lines
    yield
      if containsEOL(line) then
        lines.addAll(line.spans.flatMap: span =>
          val split = span.content.split("\n")
          split.map(Spans.styled(_, span.style))
        )
      else lines.addOne(line)
    return Text(lines.toArray)
  end toMultipleLines
