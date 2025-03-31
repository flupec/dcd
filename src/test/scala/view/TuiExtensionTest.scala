package view

import munit.FunSuite
import tui.Modifier
import tui.Rect
import tui.Span
import tui.Spans
import tui.Style

class SpansExtensionTest extends FunSuite:

  test("Bounded spans when span fits fully"):
    val spans = Spans.from(Span.nostyle("Java"))
    val rect = Rect(0, 0, 4, 2)
    val wrapped = spans.bounded(rect)
    assertEquals(wrapped.lines.length, 1)
    assertEquals(wrapped.lines(0).spans(0).content, spans.spans(0).content)

  test("Bounded spans when span must fill 2 lines"):
    val spans = Spans.from(Span.nostyle("Java"))
    val rect = Rect(0, 0, 2, 2)
    val wrapped = spans.bounded(rect)
    assertEquals(wrapped.lines.length, 2)
    assertEquals(wrapped.lines(0).spans(0).content, "Ja")
    assertEquals(wrapped.lines(1).spans(0).content, "va")

  test("Bounded spans saves style for 2 spans when all fits fully"):
    val java = Span.styled("Java", Style(addModifier = Modifier.BOLD))
    val old = Span.styled("old", Style(addModifier = Modifier.CROSSED_OUT))

    val spans = Spans.from(java, old)
    val rect = Rect(0, 0, 9, 2)
    val wrapped = spans.bounded(rect)
    assertEquals(wrapped.lines.length, 1)
    assertEquals(wrapped.lines(0).spans(0).content, java.content)
    assertEquals(wrapped.lines(0).spans(0).style, java.style)
    assertEquals(wrapped.lines(0).spans(1).content, old.content)
    assertEquals(wrapped.lines(0).spans(1).style, old.style)

  test("Bounded spans saves style for 2 spans when splits to 3 lines"):
    val java = Span.styled("Java", Style(addModifier = Modifier.BOLD))
    val old = Span.styled("old", Style(addModifier = Modifier.CROSSED_OUT))

    val spans = Spans.from(java, old)
    val rect = Rect(0, 0, 3, 2)
    val wrapped = spans.bounded(rect)
    assertEquals(wrapped.lines.length, 3)
    assertEquals(wrapped.lines(0).spans(0).content, "Jav")
    assertEquals(wrapped.lines(0).spans(0).style, java.style)
    assertEquals(wrapped.lines(1).spans(0).content, "a")
    assertEquals(wrapped.lines(1).spans(0).style, java.style)
    assertEquals(wrapped.lines(1).spans(1).content, "ol")
    assertEquals(wrapped.lines(1).spans(1).style, old.style)
    assertEquals(wrapped.lines(2).spans(0).content, "d")
    assertEquals(wrapped.lines(2).spans(0).style, old.style)
