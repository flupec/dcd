package parse

import munit.FunSuite
import java.nio.file.Path

class ParseInputTest extends FunSuite:

  test("Parse input from file should return success on existing file"):
    assert:
      ParseInput.fromFile(ParseTestUtils.resourcePathAt("dcd/settings-only.dcd")).isRight

  test("Parse input from file should return fail on non-existing file"):
    assert:
      ParseInput.fromFile(Path.of("NOT-A-FILE.dcd")).isLeft

  test("Next on EOF cursor must return EOF cursor"):
    val input = ParseInput(null, null)
    assertEquals(Cursor.EOF.next(input), Cursor.EOF)

  test("Next multi must return correct amount of existing cursors when data is enough"):
    val line = "abc"
    val input = ParseInput(null, Vector(line))
    assert(Cursor.atBegin.nextMulti(input, 3).forall(_ != Cursor.EOF))

  test("Next multi must return correct amount of non cursors when data is not enough"):
    val line = "ab"
    val input = ParseInput(null, Vector(line))
    assertEquals(Cursor.atBegin.nextMulti(input, 3).count(_ == Cursor.EOF), 1)

  test("Parse input slice when data is enough"):
    val line = "abc"
    val input = ParseInput(null, Vector(line))
    assert:
      input.slice(Cursor.atBegin, 3).forall(c => c.isDefined && line.contains(c.get))

  test("Parse input slice when data is not enough"):
    val line = "ab"
    val input = ParseInput(null, Vector(line))
    assertEquals(input.slice(Cursor.atBegin, 3).count(_.isEmpty), 1)