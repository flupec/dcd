package parse

class ParseResultTest extends munit.FunSuite:
  test("With competency should return expected success results"):
    val top = ParsedCompetency(List(1), "A", List.empty, List.empty)
    val middle1 = ParsedCompetency(List(1, 1), "A.A", List.empty, List.empty)
    val middle2 = ParsedCompetency(List(1, 2), "A.B", List.empty, List.empty)
    val low = ParsedCompetency(List(1, 1, 1), "A.A.A", List.empty, List.empty)

    locally:
      val parseResult = ParseResult(List(top), List.empty, false)
      val add = ParsedCompetency(List(2), "B", List.empty, List.empty)
      assert:
        val result = parseResult.withCompetency(add)
        result.isRight && result.right.get.competencies.size == 2

    locally:
      val parseResult = ParseResult(List(top), List.empty, false)
      assert:
        val result = parseResult.withCompetency(middle1)
        result.isRight && {
          val r = result.right.get
          r.competencies.size == 1 && r.competencies(0).childs.size == 1 && r
            .competencies(0)
            .childs(0)
            .numeration == middle1.numeration
        }

    locally:
      val parseResult = ParseResult(List(top), List.empty, false)
      assert:

        // val result = parseResult.withCompetency(middle1).flatMap(_.withCompetency(middle2))
        val result = for
          withMiddle <- parseResult.withCompetency(middle1)
          withMiddle2 <- withMiddle.withCompetency(middle2)
        yield withMiddle2

        result.isRight && {
          val r = result.right.get
          r.competencies.size == 1 && r.competencies(0).childs.size == 2 && r
            .competencies(0)
            .childs
            .count(p => p.numeration == middle1.numeration || p.numeration == middle2.numeration) == 2
        }

    locally:
      val parseResult = ParseResult(List(top), List.empty, false)
      assert:
        val result =
          parseResult.withCompetency(middle1).flatMap(_.withCompetency(middle2)).flatMap(_.withCompetency(low))
        result.isRight && {
          val r = result.right.get
          r.competencies.size == 1 && r.competencies(0).childs.size == 2 && r
            .competencies(0)
            .childs(0)
            .childs
            .exists(_.numeration == low.numeration)
        }

  test("With competency should return expected success results"):
    val parseResult = ParseResult(List.empty, List.empty, false)
    assert:
      val out = parseResult.withCompetency(ParsedCompetency(List(1), "A", List.empty, List.empty))
      out.isRight && out.right.get.competencies.size == 1

    locally:
      val out = parseResult
        .withCompetency(ParsedCompetency(List(1), "A", List.empty, List.empty))
        .flatMap(_.withCompetency(ParsedCompetency(List(1, 1), "A.A", List.empty, List.empty)))
      assert:
        out.isRight
      assert:
        val outResult = out.right.get
        outResult.competencies.size == 1 && outResult.competencies(0).childs.size == 1

  test("With competency should fail on incorrect inputs"):
    val parseResult = ParseResult(List.empty, List.empty, false)
    assert:
      parseResult.withCompetency(ParsedCompetency(List(1, 1), "A.A", List.empty, List.empty)).isLeft

  test("With competency should merge when competency already exists on top"):
    val top = ParsedCompetency(List(1), "", List.empty, List.empty)
    val parseResult = ParseResult(List(top), List.empty, false)

    val updatedTop = top.copy(qa = List(ParsedQA(question = "Question")))
    val out = parseResult.withCompetency(updatedTop)

    assert(out.isRight, out.left)
    
    val outResult = out.right.get
    assertEquals(outResult.competencies.size, 1)
    assertEquals(outResult.competencies.head.qa, updatedTop.qa)

  test("With competency should merge when competency already exists at child"):
    lazy val top = ParsedCompetency(List(1), "", List.empty, List(child))
    lazy val child = ParsedCompetency(List(1, 1), "", List.empty, List.empty)
    val parseResult = ParseResult(List(top), List.empty, false)

    val updatedChild = child.copy(qa = List(ParsedQA(question = "Question")))
    val out = parseResult.withCompetency(updatedChild)

    assert(out.isRight, out.left)
    
    val outResult = out.right.get
    assertEquals(outResult.competencies.size, 1)
    assertEquals(outResult.competencies.head.childs.size, 1)
    assertEquals(outResult.competencies.head.childs.head.qa, updatedChild.qa)
