package view

import munit.FunSuite
import tui.Spans
import tui.Text

class SpansExtensionTest extends FunSuite:

  test("Should not trim since width is enough"):
    val input = Spans.nostyle("ABCDEF")
    val maxW = 9
    assertEquals(input.trimmed(maxW).spans(0).content, input.spans(0).content)

  test("toMultipleLines should create new lines on EOL symbols in Spans"):
    val txt = Text(
      Array(
        Spans.nostyle("1"),
        Spans.nostyle("2\n3")
      )
    )
    val lines = txt.toMultipleLines.lines
    assertEquals(lines.size, 3)
