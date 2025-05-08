package result

import common.ResultExportError
import model.Competency
import model.KnowledgeComputed
import munit.FunSuite

import java.io.StringWriter
import java.io.Writer
import java.nio.file.Path

class ResultMgmtImplTest extends FunSuite:

  val Candidate = Interviewee("Avtaev")

  test("Result export must succeed"):
    val (resultTgt, descriptorTgt) = (StringWriter(), StringWriter())
    val sut = ResultMgmtImpl(
      Seq.empty,
      Candidate,
      exportDirLocator,
      exportResultLocator(resultTgt, _),
      sourceDescriptorLocator(descriptorTgt, _)
    )
    val out = sut.doExport(Vector.empty)
    assert(out.isRight)

  test("Result export must contain expected json results"):
    val (resultTgt, descriptorTgt) = (StringWriter(), StringWriter())
    val src = Vector(Competency(numeration = Vector(1), name = "name", Seq.empty, Seq.empty))
    val sut = ResultMgmtImpl(
      src,
      Candidate,
      exportDirLocator,
      exportResultLocator(resultTgt, _),
      sourceDescriptorLocator(descriptorTgt, _)
    )
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
    assertExportResult(resultTgt.toString, knowlResults)
    assertExportedDescriptor(descriptorTgt.toString, src)

  private def exportDirLocator(): Either[ResultExportError, Path] = Right(Path.of("/tmp/"))

  private def exportResultLocator(tgt: StringWriter, c: Interviewee): Either[ResultExportError, Writer] = Right(tgt)

  private def sourceDescriptorLocator(tgt: StringWriter, c: Interviewee) = exportResultLocator(tgt, c)

  private def assertExportResult(exportResult: String, inputResults: Seq[KnowledgeComputed]) =
    val r: Result = upickle.default.read(exportResult)
    assertEquals(r.candidate, Candidate)
    assert(r.sourceDescriptorHash != null)
    val knowlResults = inputResults.map(k => CompetencyKnowledgeResult(k.numeration, k.maxPoints, k.receivedPoints))
    assertEquals(knowlResults, r.competencyResults)

  private def assertExportedDescriptor(exportedDescriptor: String, input: Seq[Competency]) =
    val d: SourceDescriptor = upickle.default.read(exportedDescriptor)
    assert(d.hash != null)
    assertEquals(d.competencies.size, 1)
    input.foreach(c => assert(d.competencies.contains(c.numeration)))
