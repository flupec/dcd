package parse

import munit.FunSuite

class ParsedCompetencyTest extends FunSuite:
  test("Should insert successfully for correct inputs"):
    val top = ParsedCompetency(List(1), "A", List.empty, List.empty)

    val middle1 = ParsedCompetency(List(1, 1), "A.A", List.empty, List.empty)
    val middle2 = ParsedCompetency(List(1, 2), "A.B", List.empty, List.empty)

    val low = ParsedCompetency(List(1, 1, 1), "A.A.A", List.empty, List.empty)

    assert:
      val res = top.withInserted(middle1)
      res.isRight && res.right.get.childs.find(c => c.numeration == middle1.numeration).isDefined

    assert:
      val res = top.withInserted(middle2)
      res.isRight && res.right.get.childs.find(c => c.numeration == middle2.numeration).isDefined

    locally:
      val res = top.withInserted(middle1).flatMap(_.withInserted(low))
      assert(res.isRight)
      val middleFound = res.right.get.childs.find(c => c.numeration == middle1.numeration)
      assert(middleFound.isDefined)
      val lowFound = middleFound.get.childs.find(c => c.numeration == low.numeration)
      assert(lowFound.isDefined)

  test("Insert should fail on incorrect and inputs"):
    val top = ParsedCompetency(List(1), "A", List.empty, List.empty)
    val middle = ParsedCompetency(List(1, 1), "A.A", List.empty, List.empty)
    val low = ParsedCompetency(List(1, 1, 1), "A.A.A", List.empty, List.empty)

    assert:
      val incorrect = ParsedCompetency(List.empty, "", List.empty, List.empty)
      top.withInserted(incorrect).isLeft

    assert:
      val noParent = ParsedCompetency(List(2, 1), "B.A", List.empty, List.empty)
      top.withInserted(noParent).isLeft
      middle.withInserted(noParent).isLeft
      low.withInserted(noParent).isLeft

    assert:
      val noParent = ParsedCompetency(List(2, 1, 1), "B.A.A", List.empty, List.empty)
      middle.withInserted(low).isLeft
      top.withInserted(middle).flatMap(_.withInserted(noParent)).isLeft

  test("Find by numeration success paths"):
    val top = ParsedCompetency(List(1), "A", List.empty, List.empty)
    val middle = ParsedCompetency(List(1, 1), "A.A", List.empty, List.empty)
    val low = ParsedCompetency(List(1, 1, 1), "A.A.A", List.empty, List.empty)

    val tree = top.withInserted(middle).flatMap(_.withInserted(low)).right.get

    assert:
      val found = ParsedCompetency.findByNumeration(tree, top.numeration)
      found.isDefined && found.get.numeration == top.numeration

    assert:
      val found = ParsedCompetency.findByNumeration(tree, middle.numeration)
      found.isDefined && found.get.numeration == middle.numeration

    assert:
      val found = ParsedCompetency.findByNumeration(tree, low.numeration)
      found.isDefined && found.get.numeration == low.numeration
