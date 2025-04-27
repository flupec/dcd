package result

import common.ResultExportError
import model.KnowledgeComputed
import munit.FunSuite

import java.io.StringWriter
import java.io.Writer
import java.nio.file.Path

class ResultMgmtImplTest extends FunSuite:

  val Source = Path.of("example.dcd")
  val Candidate = Interviewee("Avtaev")

  test("Result export must suceed"):
    val exportTgt = StringWriter()
    val sut = ResultMgmtImpl(Path.of("example.dcd"), Candidate, exportTgtLocator(exportTgt, _))
    val out = sut.doExport(Vector.empty)
    assert(out.isRight)

  test("Result export must contain expected json results"):
    val exportTgt = StringWriter()
    val sut = ResultMgmtImpl(Path.of("example.dcd"), Candidate, exportTgtLocator(exportTgt, _))
    val knowlResults = Vector(
      KnowledgeComputed(
        numeration = Vector(1),
        maxPoints = 1f,
        receivedPoints = 0.5f,
        overridenBy = None,
        synthetic = false
      )
    )
    val out = sut.doExport(knowlResults)
    assert(out.isRight)
    assertExportResult(exportTgt.toString, knowlResults)

  private def exportTgtLocator(tgt: StringWriter, c: Interviewee): Either[ResultExportError, Writer] = Right(tgt)

  private def assertExportResult(exportResult: String, inputResults: Seq[KnowledgeComputed]) =
    val r: Result = upickle.default.read(exportResult)
    assertEquals(r.candidate, Candidate)
    assertEquals(r.sourceFilename, Source.toString)
    val knowlResults = inputResults.map(k => CompetencyKnowledgeResult(k.numeration, k.maxPoints, k.receivedPoints))
    assertEquals(knowlResults, r.competencyResults)
