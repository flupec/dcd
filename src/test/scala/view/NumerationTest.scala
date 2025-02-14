package view

import munit.FunSuite

class NumerationTest extends FunSuite:
  test("Next numeration for empty should return empty numeration"):
    assert(next(List.empty).isEmpty)

  test("Next numeration for single-sized should return correct next numeration"):
    val inputs = for num <- 0 until 10 yield List(num)
    for currNumeration <- inputs do assert(next(currNumeration).last == currNumeration.last + 1)

  test("Next numeration for double-sized should return correct next numeration"):
    val inputs = for num <- 0 until 10 yield List(0, num)
    for currNumeration <- inputs do assert(next(currNumeration).last == currNumeration.last + 1)

  test("Prev numeration for empty should return empty numeration"):
    assert(previous(List.empty).isEmpty)

  test("Prev numeration for single-sized should return correct next numeration"):
    val inputs = for num <- 2 until 10 yield List(num)
    for currNumeration <- inputs do assert(previous(currNumeration).last == currNumeration.last - 1)

  test("Prev numeration should not return numeration with negative items"):
    val inputs = List(
      List(1),
      List(1, 1),
      List(1, 1, 1)
    )
    for currNumeration <- inputs do assert(previous(currNumeration) == currNumeration)

  test("Child of must return true for direct childs"):
    val parentsChilds = List(
      List(1) -> List(1, 1),
      List(1) -> List(1, 2),
      List(1, 2) -> List(1, 2, 1),
      List(1, 2) -> List(1, 2, 2),
      List(1, 2) -> List(1, 2, 3)
    )
    for (parent, child) <- parentsChilds do assert(child.isChildOf(parent))

  test("Child of must return false for non-childs"):
    val parentsAndSomething = List(
      List(1) -> List(1),
      List(1) -> List(2),
      List(1, 2) -> List(1, 2),
      List(1, 2) -> List(1),
      List(1, 2, 3) -> List(1, 2)
    )
    for (parent, something) <- parentsAndSomething do assert(!something.isChildOf(parent))
  
  test("Child of must return true for transitive childs"):
    val parentsChilds = List(
      List(1) -> List(1, 1, 1),
      List(1) -> List(1, 2, 1),
      List(1) -> List(1, 2, 3),
      List(1, 2) -> List(1, 2, 3, 4),
      List(1, 2) -> List(1, 2, 3, 10),
      List(1, 2) -> List(1, 2, 3)
    )
    for (parent, child) <- parentsChilds do assert(child.isChildOf(parent))

  test("Child of must return false for incorrect transitive childs"):
    val parentsSomething = List(
      List(1) -> List(2, 1, 1),
      List(1, 2) -> List(2, 2, 3, 4),
      List(1, 2) -> List(1, 3, 4, 5),
    )
    for (parent, smth) <- parentsSomething do assert(!smth.isChildOf(parent))