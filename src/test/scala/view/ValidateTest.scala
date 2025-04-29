package view

import munit.FunSuite

class ValidateTest extends FunSuite:

  test("Not empty validator should return left on empty input"):
    val input = ""
    val result = notEmptyValidator(input)
    assert(result.isLeft)

  test("Not empty validator should return right on non-empty input"):
    val input = "not-empty"
    val result = notEmptyValidator(input)
    assert(result.isRight)
  
  test("Num range validator should return left on incorrect inputs"):
    val (min, max) = (0, 100)
    val inputs = List(String.valueOf(min - 1), String.valueOf(max + 1))
    val allLeft = inputs.map(numRangeValidator(min, max)(_)).forall(_.isLeft)
    assert(allLeft)

  test("Num range validator should return right on correct inputs"):
    val (min, max) = (0, 100)
    val inputs = (0 to max).map(String.valueOf)
    val allRight = inputs.map(numRangeValidator(min, max)(_)).forall(_.isRight)
    assert(allRight)
  
  test("Only digits validator should return left on character symbols"):
    val inputs = List(
      "qwerty",
      "123qwerty",
      "qwerty123",
      "-4",
      "  ",
      "\n"
    )
    val allLeft = inputs.map(onlyDigitsValidator).forall(_.isLeft)
    assert(allLeft)

  test("Only digits validator should return right on digit symbols"):
    val inputs = List(
      "1",
      "123",
      "0123"
    )
    val allRight = inputs.map(onlyDigitsValidator).forall(_.isRight)
    assert(allRight)

  test("Composite validator should act in expected manner"):
    val validator = composition(notEmptyValidator, onlyDigitsValidator, numRangeValidator(1, 5))
    assert(validator("").isLeft)
    assert(validator("qwe").isLeft)
    assert(validator("10").isLeft)
    assert(validator("3").isRight)