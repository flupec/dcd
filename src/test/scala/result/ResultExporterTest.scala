package result

import common.ResultExportError
import model.Competency
import model.KnowledgeComputed
import munit.FunSuite

import java.io.StringWriter
import java.io.Writer
import java.nio.file.Path

class ResultExporterTest extends FunSuite:

  val Candidate = Interviewee("Avtaev")

  test("Result export must succeed"):
    val (resultTgt, descriptorTgt) = (StringWriter(), StringWriter())
    val sut = ResultExporter(
      Seq.empty,
      Candidate,
      exportDirLocator,
      exportResultLocator(resultTgt, _),
      sourceDescriptorLocator(descriptorTgt, _)
    )
    val out = sut.doExport(Vector.empty, Vector.empty, Vector.empty)
    assert(out.isRight)

  test("Result export must contain expected json results in knowl results"):
    val (resultTgt, descriptorTgt) = (StringWriter(), StringWriter())
    val src = Vector(Competency(numeration = Vector(1), name = "name", Seq.empty, Seq.empty))
    val sut = ResultExporter(
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
    val out = sut.doExport(knowlResults, Vector.empty, Vector.empty)
    assert(out.isRight)
    assertExportResult(resultTgt.toString, knowlResults)
    assertExportedDescriptor(descriptorTgt.toString, src)

  test("ResultExporter constructor must generate different hash for different competency sources"):
    val (resultTgt, descriptorTgt) = (StringWriter(), StringWriter())
    val src1 = Vector(Competency(numeration = Vector(1), name = "name1", Seq.empty, Seq.empty))
    val src2 = Vector(Competency(numeration = Vector(1), name = "name2", Seq.empty, Seq.empty))
    val descriptor1Hash = ResultExporter(
      src1,
      Candidate,
      exportDirLocator,
      exportResultLocator(resultTgt, _),
      sourceDescriptorLocator(descriptorTgt, _)
    ).sourceDescriptor.hash
    val descriptor2Hash = ResultExporter(
      src2,
      Candidate,
      exportDirLocator,
      exportResultLocator(resultTgt, _),
      sourceDescriptorLocator(descriptorTgt, _)
    ).sourceDescriptor.hash
    assertNotEquals(descriptor1Hash, descriptor2Hash)

  test("Result export must contain expected json results in note results"):
    val (resultTgt, descriptorTgt) = (StringWriter(), StringWriter())
    val competency =
      Competency(numeration = Vector(1), name = "name", Seq.empty, Seq.empty, notes = Vector("First", "Second"))
    val src = Vector(competency)
    val sut = ResultExporter(
      src,
      Candidate,
      exportDirLocator,
      exportResultLocator(resultTgt, _),
      sourceDescriptorLocator(descriptorTgt, _)
    )
    val expectedNotesResult = CompetencyNoteResult(Vector(1), competency.notes)
    val out = sut.doExport(List.empty, Vector.empty, Vector(expectedNotesResult))
    assert(out.isRight)
    assertNotesResult(resultTgt.toString, Vector(expectedNotesResult))

  private def exportDirLocator(): Either[ResultExportError, Path] = Right(Path.of("/tmp/"))

  private def exportResultLocator(tgt: StringWriter, c: Interviewee): Either[ResultExportError, Writer] = Right(tgt)

  private def sourceDescriptorLocator(tgt: StringWriter, c: Interviewee) = exportResultLocator(tgt, c)

  private def assertExportResult(exportResult: String, inputResults: Seq[KnowledgeComputed]) =
    val r: Result = upickle.default.read(exportResult)
    assertEquals(r.candidate, Candidate)
    assert(r.sourceDescriptorHash != null)
    val knowlResults = inputResults.map(k => CompetencyKnowledgeResult(k.numeration, k.maxPoints, k.receivedPoints))
    assertEquals(knowlResults, r.competencyResults)

  private def assertNotesResult(exportResult: String, inputNoteResults: Seq[CompetencyNoteResult]) =
    val r: Result = upickle.default.read(exportResult)
    assertEquals(r.candidate, Candidate)
    assert(r.sourceDescriptorHash != null)
    assertEquals(r.noteResults, inputNoteResults)

  private def assertExportedDescriptor(exportedDescriptor: String, input: Seq[Competency]) =
    val d: SourceDescriptor = upickle.default.read(exportedDescriptor)
    assert(d.hash != null)
    assertEquals(d.competencies.size, 1)
    input.foreach(c => assert(d.competencies.contains(c.numeration)))
