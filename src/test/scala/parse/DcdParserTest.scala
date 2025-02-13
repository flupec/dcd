package parse

import munit.FunSuite

class DcdParserTest extends FunSuite:

  test("Should parse settings in settings-only.dcd file"):
    val input = ParseTestUtils.parseInputAt("dcd/settings-only.dcd")
    val out = Parser.parse(input)
    assert(out.isRight, out.left)

    val parseResult = out.right.get
    assert(parseResult.settings.nonEmpty)
    assert(parseResult.settings.contains("format", "dcd"))
    assert(parseResult.settings.contains("setting", "value"))

  test("Should parse competency headers in only-competency-headers.dcd"):
    val input = ParseTestUtils.parseInputAt("dcd/only-competency-headers.dcd")
    val out = Parser.parse(input)
    assert(out.isRight, out.left)

    val parseResult = out.right.get
    assertHeaders(parseResult)

  test("Should parse competency questions in without-answers.dcd"):
    val input = ParseTestUtils.parseInputAt("dcd/without-answers.dcd")
    val out = Parser.parse(input)
    println(out.right)
    assert(out.isRight, out.left)

    val parseResult = out.right.get
    assertHeaders(parseResult)
    assertQuestions(parseResult)

  test("Should parse competency answers in with-answers.dcd"):
    val input = ParseTestUtils.parseInputAt("dcd/with-answers.dcd")
    val out = Parser.parse(input)
    println(out)
    assert(out.isRight, out.left)

    val parseResult = out.right.get
    assertHeaders(parseResult)
    assertQuestions(parseResult)
    assertAnswers(parseResult)

  private def assertHeaders(parseResult: ParseResult) =
    // Programming header assertions
    assert(
      parseResult.competencies.exists(c => c.numeration == List(1) && c.name == "Programming")
    )

    val programmingMidLayer = parseResult.competencies
      .find(c => c.numeration == List(1))
      .map(c => c.childs)
      .getOrElse(List.empty)

    assert(
      programmingMidLayer.exists(c => c.numeration == List(1, 1) && c.name == "Variables")
    )
    assert(
      programmingMidLayer.exists(c => c.numeration == List(1, 2) && c.name == "Control structures")
    )

    val programmingLowLayer = parseResult
      .competencies(0)
      .childs
      .find(c => c.numeration == List(1, 2))
      .map(c => c.childs)
      .getOrElse(List.empty)

    assert(
      programmingLowLayer.exists(c => c.numeration == List(1, 2, 1) && c.name == "If statements (examples)")
    )
    assert(
      programmingLowLayer.exists(c => c.numeration == List(1, 2, 2) && c.name == "Loops (examples)")
    )

    // Java header assertions
    assert(
      parseResult.competencies
        .exists(c => c.numeration == List(2) && c.name == "Java")
    )

    val javaLowLayer = parseResult.competencies
      .find(c => c.numeration == List(2))
      .map(c => c.childs)
      .getOrElse(List.empty)

    assert(
      javaLowLayer.exists(c => c.numeration == List(2, 1) && c.name == "Exception hierarchy")
    )
    assert(
      javaLowLayer.exists(c => c.numeration == List(2, 2) && c.name == "Collections API")
    )
  end assertHeaders

  private def assertQuestions(parseResult: ParseResult) =
    assert(
      parseResult.competencies
        .flatMap(c => c.qa)
        .exists(qa => qa.question == "Definition")
    )

    val variables = parseResult.competencies
      .flatMap(_.childs)
      .find(c => c.numeration == List(1, 1))
      .get

    assert(
      variables.qa.exists(c => c.question == "Local variables")
        && variables.qa.exists(c => c.question == "Global variables")
        && variables.qa.exists(c => c.question == "Scope of variable")
    )

    val controlStructures = parseResult.competencies
      .flatMap(_.childs)
      .find(c => c.numeration == List(1, 2))
      .get

    assert(controlStructures.qa.isEmpty)

    val java = parseResult.competencies
      .find(c => c.numeration == List(2))
      .get

    assert(java.qa.isEmpty)

    val exceptionHierarchy = parseResult.competencies
      .flatMap(_.childs)
      .find(c => c.numeration == List(2, 1))
      .get

    assert(exceptionHierarchy.qa.isEmpty)

    val collectionsApi = parseResult.competencies
      .flatMap(_.childs)
      .find(c => c.numeration == List(2, 2))
      .get

    assert(
      collectionsApi.qa.exists(c => c.question == "How generics used in collections API")
        && collectionsApi.qa.exists(c => c.question == "List API")
        && collectionsApi.qa.exists(c => c.question == "Map API")
    )

  private def assertAnswers(parseResult: ParseResult) =
    assert:
      parseResult.competencies
        .flatMap(c => c.qa)
        .exists(qa =>
          qa.answer.get == """Computer programming or coding is the composition of sequences of instructions,
                                          |called programs, that computers can follow to perform tasks""".stripMargin
        )

    val variables = parseResult.competencies
      .flatMap(_.childs)
      .find(c => c.numeration == List(1, 1))
      .get

    assert:
      variables.qa.exists(c =>
        c.question == "Local variables"
          && c.answer.isDefined
          && c.answer.get == "A local variable is a variable that is given local scope"
      )

    assert:
      variables.qa.exists(c =>
        c.question == "Global variables"
          && c.answer.isDefined
          && c.answer.get == "A global variable is a variable with global scope"
      )
