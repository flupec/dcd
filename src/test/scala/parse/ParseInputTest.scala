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
