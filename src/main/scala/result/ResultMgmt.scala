package result

import common.EitherExtension.sequenceRight
import common.ResultExportError
import common.ResultImportError
import model.Competency
import model.KnowledgeComputed
import model.QA
import result.ResultExporter.toDescriptor
import upickle.default.read
import upickle.default.writeTo

import java.io.File
import java.io.FileWriter
import java.io.Writer
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Base64
import scala.annotation.tailrec
import scala.util.Try
import scala.util.Using

type Importer = (results: Seq[File], descriptor: File) => Either[ResultImportError, (Seq[Result], SourceDescriptor)]

// Responsible for export path determination.  Used for different path resolving strategies and testing
type ExportDirLocator = () => Either[ResultExportError, Path]

// Responsible for export target file determination
type ExportTgtLocator = (c: Interviewee) => Either[ResultExportError, Writer]

private val ExportDirEnvKey = "DCD_EXPORT_PATH"

def fsExportDirLocator: Either[ResultExportError, Path] = Right(
  Path.of(sys.env.get(ExportDirEnvKey).getOrElse(sys.props("user.dir")))
)

def exportResultLocator(l: ExportDirLocator): ExportTgtLocator = c =>
  l.apply.flatMap: exportDir =>
    val timestamp: String = OffsetDateTime.now.format(DateTimeFormatter.ISO_LOCAL_DATE)
    val filename = s"${c.lastname}-$timestamp.json"
    Right(FileWriter(exportDir.resolve(filename).toFile))

def sourceDescriptorLocator(l: ExportDirLocator, sourceHash: String): ExportTgtLocator = c =>
  l.apply.flatMap: exportDir =>
    val shortenedHash = sourceHash.substring(0, sourceHash.length.min(6))
    val filename = s"descriptor-$shortenedHash.json"
    Right(FileWriter(exportDir.resolve(filename).toFile))

class ResultExporter(
    /** Descriptor with initial information about competencies, obtained from dcd file */
    val sourceDescriptor: SourceDescriptor,
    /** Candidate person data */
    val candidate: Interviewee,
    private val resultLocator: ExportTgtLocator,
    private val descriptorLocator: ExportTgtLocator
):

  /** Perform knowledge data export. Knowledge data is acquired at interviewee estimate session
    *
    * @param competenciesKnowledge competency knowledges
    * @param qaKnowledges qa knowledges
    * @param notes notes
    * @param allCompetencies all competencies. May not equal to competencies in dcd file
    */
  def doExport(
      competenciesKnowledge: Seq[KnowledgeComputed],
      qaKnowledges: Seq[QAKnowledgeResult],
      notes: Seq[CompetencyNoteResult],
      allCompetencies: Seq[Competency]
  ): Either[ResultExportError, Unit] =
    val result = exportResult(competenciesKnowledge, qaKnowledges, notes, allCompetencies)
    for
      descriptorWriter <- descriptorLocator(result.candidate)
      resultWriter <- resultLocator(result.candidate)
    yield exportResultWithDescriptor(result, sourceDescriptor, resultWriter, descriptorWriter)

  private def exportResultWithDescriptor(
      result: Result,
      desc: SourceDescriptor,
      resultWriter: Writer,
      descWriter: Writer
  ): Either[ResultExportError, Unit] =
    val written = Using.Manager: use =>
      val descFw = use(descWriter)
      writeTo(desc, descFw, indent = 2)
      val resFw = use(resultWriter)
      writeTo(result, resFw, indent = 2)
    return written.toEither.left.map(e => ResultExportError.FileOpErr(e.getMessage))

  private def exportResult(
      ks: Seq[KnowledgeComputed],
      qaKnowledges: Seq[QAKnowledgeResult],
      notes: Seq[CompetencyNoteResult],
      allCompetencies: Seq[Competency]
  ) =
    val extraCompetencies = allCompetencies
      .filter(c => !sourceDescriptor.competencies.contains(c.numeration))
      .map(c => c.numeration -> toDescriptor(c))
      .toMap
    Result(
      sourceDescriptorHash = sourceDescriptor.hash,
      candidate = candidate,
      competencyResults = ks.map(toKnowledgeResult(_)),
      qaResults = qaKnowledges,
      noteResults = notes,
      extraCompetencies = extraCompetencies
    )

  private def toKnowledgeResult(k: KnowledgeComputed) = CompetencyKnowledgeResult(
    numeration = k.numeration,
    maxPoints = k.maxPoints,
    receivedPoints = k.receivedPoints
  )
end ResultExporter

object ResultExporter:

  // We want to use base64 encoded hash as substring in filename but, for example, '/' is path separator, so replace it
  // Replaceable -> (replaceable, replacer) mapping
  private val Base64BannedChars = Map(
    "/" -> ("/", "-")
  )

  def apply(
      sourceCompetencies: Seq[Competency],
      candidate: Interviewee,
      dirLocator: ExportDirLocator = () => fsExportDirLocator
  ) =
    val hash = computeHash(sourceCompetencies)
    val sourceDescriptor = competencyDescriptor(sourceCompetencies, hash)
    val resultLocator = exportResultLocator(dirLocator)
    val descriptorLocator = sourceDescriptorLocator(dirLocator, hash)
    new ResultExporter(sourceDescriptor, candidate, resultLocator, descriptorLocator)

  def apply(
      sourceCompetencies: Seq[Competency],
      candidate: Interviewee,
      dirLocator: ExportDirLocator,
      resultLocator: ExportTgtLocator,
      descriptorLocator: ExportTgtLocator
  ) =
    val hash = computeHash(sourceCompetencies)
    val sourceDescriptor = competencyDescriptor(sourceCompetencies, hash)
    new ResultExporter(sourceDescriptor, candidate, resultLocator, descriptorLocator)

  private def computeHash(source: Seq[Competency]): String =
    val hashTarget = source.map(_.hashTarget).mkString
    val hash = MessageDigest.getInstance("SHA-256").digest(hashTarget.getBytes(StandardCharsets.UTF_8))
    val b64Hash = Base64.getEncoder.encodeToString(hash)
    return replaceBannedChars(b64Hash, Base64BannedChars)

  private def competencyDescriptor(sourceCompetencies: Seq[Competency], hash: String) =
    val flattenSource = sourceCompetencies.flatMap(_.flatten)
    SourceDescriptor(hash = hash, competencies = flattenSource.map(c => (c.numeration, toDescriptor(c))).toMap)

  private def toDescriptor(c: Competency) = CompetencyDescriptor(
    name = c.name,
    qa = toQADescriptors(c.qa)
  )

  private def toQADescriptors(qs: Seq[QA]) = qs.map(q => QADescriptor(question = q.question, answer = q.answer))

  @tailrec
  private def replaceBannedChars(tgt: String, replacings: Map[String, (String, String)]): String =
    if replacings.isEmpty then tgt
    else
      val anyReplacing = replacings.head
      val (replaceable, replace) = anyReplacing._2
      replaceBannedChars(tgt.replaceAll(replaceable, replace), replacings removed anyReplacing._1)

  // TODO
  // @tailrec
  // private def restoreBannedChars(tgt: String, restoreWith: Map[String, (String, String)]): String = ???
end ResultExporter

object ResultImporter:
  def doImport(results: Seq[File], descriptor: File): Either[ResultImportError, (Seq[Result], SourceDescriptor)] =
    val rs: Seq[Try[Result]] = results.map(f => Try(read(f)))
    val desc: Try[SourceDescriptor] = Try(read(descriptor))
    for
      r <- rs.map(_.toEither).sequenceRight.left.map(e => ResultImportError.FileOpErr(e.getMessage))
      d <- desc.toEither.left.map(e => ResultImportError.FileOpErr(e.getMessage))
    yield ((r -> d))
  end doImport
end ResultImporter
