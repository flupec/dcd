package view

import munit.FunSuite
import tui.Spans

class SpansExtensionTest extends FunSuite:

  test("Should not trim since width is enough"):
    val input = Spans.nostyle("ABCDEF")
    val maxW = 9
    assertEquals(input.trimmed(maxW).spans(0).content, input.spans(0).content)
