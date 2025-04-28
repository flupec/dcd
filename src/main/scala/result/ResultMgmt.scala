package result

import common.ResultExportError
import common.ResultImportError
import model.KnowledgeComputed
import upickle.default.writeTo

import java.io.File
import java.io.FileWriter
import java.io.Writer
import java.nio.file.Path
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import scala.util.Using

/** Responsible for import/export of competency estimation results */
trait ResultMgmt:

  /** Export competency estimation results
    *
    * @param tgt export target
    */
  def doExport(tgt: Seq[KnowledgeComputed]): Either[ResultExportError, Unit]

  /** Import competency estimation results
    *
    * @param from import file
    */
  def doImport(from: File): Either[ResultImportError, Seq[KnowledgeComputed]]

// Responsible for export path determination. Used for different path resolving strategies and testing
type ExportTgtLocator = (c: Interviewee) => Either[ResultExportError, Writer]

private val ExportDirEnvKey = "DCD_EXPORT_PATH"

def fsExportTgtLocator(c: Interviewee): Either[ResultExportError, Writer] =
  val exportDir = Path.of(sys.env.get(ExportDirEnvKey).getOrElse(sys.props("user.dir")))
  val timestamp: String = OffsetDateTime.now.format(DateTimeFormatter.ISO_OFFSET_DATE)
  val filename = s"${c.lastname}-$timestamp.json"
  return Right(FileWriter(exportDir.resolve(filename).toFile))

class ResultMgmtImpl(
    private val competencySourceFile: Path,
    private val candidate: Interviewee,
    private val exportLocator: ExportTgtLocator = fsExportTgtLocator
) extends ResultMgmt:

  override def doExport(tgt: Seq[KnowledgeComputed]): Either[ResultExportError, Unit] =
    val result = exportResult(tgt)
    return exportLocator(result.candidate) match
      case Left(err) => Left(err)
      case Right(exportTgt) =>
        Using(exportTgt)(fw => writeTo(result, fw)).toEither
          .fold(e => Left(ResultExportError.FileOpErr(e.getMessage)), _ => Right(()))
  end doExport

  private def exportResult(ks: Seq[KnowledgeComputed]) = Result(
    sourceFilename = competencySourceFile.toString,
    candidate = candidate,
    competencyResults = ks.map(toKnowledgeResult(_))
  )

  private def toKnowledgeResult(k: KnowledgeComputed) = CompetencyKnowledgeResult(
    numeration = k.numeration,
    maxPoints = k.maxPoints,
    receivedPoints = k.receivedPoints
  )

  override def doImport(from: File): Either[ResultImportError, Seq[KnowledgeComputed]] = ???
end ResultMgmtImpl
