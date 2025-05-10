package result

import com.lowagie.text.Cell
import com.lowagie.text.ChapterAutoNumber
import com.lowagie.text.Document
import com.lowagie.text.Image
import com.lowagie.text.Table
import com.lowagie.text.pdf.PdfWriter
import common.EitherExtension.sequenceRight
import common.ReportError
import common.directParent
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.SpiderWebPlot
import org.jfree.data.category.DefaultCategoryDataset
import result.ReportGenerator.fileReportTarget

import java.awt.Color
import java.awt.Font
import java.io.FileOutputStream
import java.io.OutputStream
import java.nio.file.Path
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object ReportGenerator:

  val IndividualChartSize = (480, 480)

  type ReportTgtProvider = () => OutputStream
  type IndividualReportGenerator = (r: Result, s: SourceDescriptor) => Either[ReportError, Unit]
  type ComparisonReportGenerator = (rs: Seq[Result], s: SourceDescriptor) => Either[ReportError, Unit]

  def fileReportTarget(filepath: Path): ReportTgtProvider = () => FileOutputStream(filepath.toFile)

  def generate(
      src: Seq[(Result, SourceDescriptor)],
      targetProvider: ReportTgtProvider
  ): Either[ReportError, Unit] =
    for
      pdf <- initPdfTarget(targetProvider)
      _ <- generateIndividualReport(individualGenerator(pdf), src)
      _ = pdf.close()
    yield ()
  end generate

  private def initPdfTarget(tgtProvider: ReportTgtProvider): Either[ReportError, Document] =
    val pdf = Document()
    Try(PdfWriter.getInstance(pdf, tgtProvider())) match
      case Failure(_) => Left(ReportError.GenerateError)
      case Success(_) =>
        pdf.open()
        Right(pdf)
  end initPdfTarget

  private def generateIndividualReport(
      generator: IndividualReportGenerator,
      src: Seq[(Result, SourceDescriptor)]
  ): Either[ReportError, Unit] =
    src.map((r, s) => generator(r, s)).sequenceRight.map(_ => ())

  private def generateComparisonReport(
      generator: ComparisonReportGenerator,
      src: Seq[(Result, SourceDescriptor)]
  ): Either[ReportError, Unit] = ???

  private def individualGenerator(pdf: Document): IndividualReportGenerator = (r, s) =>
    val resultsByNestLevel = r.competencyResults.groupBy(_.numeration.size)
    resultsByNestLevel.values
      .map(generateCompetencyChart(_, r, s))
      .toSeq
      .sequenceRight
      .map(insertIndividualReports(pdf, _, r))

  private def generateCompetencyChart(
      sameLvlKnowledges: Seq[CompetencyKnowledgeResult],
      r: Result,
      s: SourceDescriptor
  ): Either[ReportError, IndividualReport] =
    // Parent competency. Will be ther same for all sameLvlKnowledges items
    val parentCompetency = sameLvlKnowledges.head.numeration.directParent
      .flatMap(s.competencies.get(_))
      .map(_.name)
      .getOrElse("Overview")
    return sameLvlKnowledges
      .map: k =>
        // Get competency name for knowledge
        s.competencies
          .get(k.numeration)
          .map(_.name)
          .fold(Left(ReportError.GenerateError))(Right(_))
          .map(name => (k.receivedPoints, name))
      .sequenceRight
      .map: knowlData =>
        // TODO if knowlData has less than 3 items, then add 2 or 1 fake item to see good SpiderWebPlot, not line or point
        val ds = DefaultCategoryDataset()
        // Add value to plot for each knowledge
        knowlData.foreach((points, competency) => ds.addValue(points, r.candidate.lastname, competency))
        val plot = SpiderWebPlot(ds)
        plot.setLabelFont(Font(Font.MONOSPACED, Font.BOLD, 16))
        IndividualReport.CompetencySpider(parentCompetency, JFreeChart(plot))
  end generateCompetencyChart

  private def insertIndividualReports(
      pdf: Document,
      reports: Seq[IndividualReport],
      r: Result
  ): Either[ReportError, Unit] =
    val competencySpiderCharts = reports.collect({ case c @ IndividualReport.CompetencySpider(_, _, _) => c })
    insertIndividualChartTable(pdf, competencySpiderCharts, r)

  private def insertIndividualChartTable(
      pdf: Document,
      charts: Seq[IndividualReport.CompetencySpider],
      r: Result
  ): Either[ReportError, Unit] =
    charts
      .map(c => getPdfImage(c.chart, IndividualChartSize).map(img => c.copy(chartImg = Some(img))))
      .sequenceRight
      .map: reports =>
        val table = constructTable(reports)
        pdf.add(ChapterAutoNumber(s"${r.candidate.lastname} competencies"))
        pdf.add(table)
        ()
  end insertIndividualChartTable

  private def getPdfImage(c: JFreeChart, sz: (Int, Int)): Either[ReportError, Image] =
    val img = c.createBufferedImage(sz._1, sz._2)
    return Try(Image.getInstance(img, null)).toEither.left
      .map(_ => ReportError.GenerateError)
  end getPdfImage

  private def constructTable(reports: Seq[IndividualReport.CompetencySpider]): Table =
    val table = Table(2)
    table.setBorderColor(Color.WHITE)
    table.setSpacing(5f)
    reports.foreach: report =>
      val competencyCell = Cell(report.competency)
      competencyCell.setWidth("5%")
      val imgCell = Cell(report.chartImg.get)
      imgCell.setWidth("95%")
      table.addCell(competencyCell)
      table.addCell(imgCell)
    return table

  private enum IndividualReport:
    case CompetencySpider(competency: String, chart: JFreeChart, chartImg: Option[Image] = None)
end ReportGenerator

// @main def main =
//   val ds = DefaultCategoryDataset()
//   ds.addValue(95, "Avtaev", "Programming")
//   ds.addValue(75, "Avtaev", "Social skills")
//   ds.addValue(40, "Avtaev", "Network")

//   ds.addValue(40, "Smith", "Programming")
//   ds.addValue(50, "Smith", "Social skills")
//   ds.addValue(60, "Smith", "Network")

//   val plt = SpiderWebPlot(ds)
//   plt.setMaxValue(100f)
//   val chart = JFreeChart(plt)

//   // ChartUtils.saveChartAsPNG(Path.of("/tmp", "test.png").toFile, chart, 1280, 720)

//   val img = chart.createBufferedImage(480, 320)

//   val pdf = Document()
//   PdfWriter.getInstance(pdf, FileOutputStream(Path.of("/tmp/test.pdf").toFile))
//   pdf.open()

//   val imagesTable = Table(2)
//   imagesTable.setBorderColor(Color.WHITE)
//   imagesTable.setSpacing(5f)
//   imagesTable.addCell(Cell(Image.getInstance(img, null)))
//   imagesTable.addCell(Cell(Image.getInstance(img, null)))
//   pdf.add(ChapterAutoNumber("First comparison"))
//   pdf.add(imagesTable)
//   pdf.add(ChapterAutoNumber("Second comparison"))
//   pdf.add(imagesTable)

//   pdf.close()

@main def main =
  val imported = ResultMgmtImpl.doImport(
    Path.of("/home/flupec/coding-projects/dcd/dcd/Avtaev-2025-05-10.json").toFile,
    Path.of("/home/flupec/coding-projects/dcd/dcd/descriptor-9KsLOY.json").toFile
  )
  imported match
    case Left(value) => println(s"Import error! $value")
    case Right((result, source)) =>
      ReportGenerator.generate(Vector((result, source)), fileReportTarget(Path.of("/tmp/test.pdf"))) match
        case Left(value)  => println(s"Error! Error=$value")
        case Right(value) => println("Success")
