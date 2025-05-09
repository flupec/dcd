package result

import com.lowagie.text.Cell
import com.lowagie.text.Document
import com.lowagie.text.Element
import com.lowagie.text.ElementTags
import com.lowagie.text.Image
import com.lowagie.text.Paragraph
import com.lowagie.text.Table
import com.lowagie.text.pdf.PdfWriter
import common.EitherExtension.sequenceRight
import common.ReportError
import common.directParent
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.SpiderWebPlot
import org.jfree.chart.title.TextTitle
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

  val IndividualChartSize = (200, 200)

  type ReportTgtProvider = () => OutputStream
  type IndividualReportGenerator = (r: Result, s: SourceDescriptor) => Either[ReportError, Unit]
  type ComparisonReportGenerator = (rs: Seq[Result], s: SourceDescriptor) => Either[ReportError, Unit]

  def fileReportTarget(filepath: Path): ReportTgtProvider = () => FileOutputStream(filepath.toFile)

  def generate(
      results: Seq[Result],
      source: SourceDescriptor,
      targetProvider: ReportTgtProvider
  ): Either[ReportError, Unit] =
    for
      pdf <- initPdfTarget(targetProvider)
      _ <- generateIndividualReport(individualGenerator(pdf), results, source)
      // TODO IMPL ME
      // _ <- generateComparisonReport(comparisonGenerator(pdf), results, source)
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
      src: Seq[Result],
      descriptor: SourceDescriptor
  ): Either[ReportError, Unit] = src.map(generator(_, descriptor)).sequenceRight.map(_ => ())

  // TODO IMPL ME
  private def generateComparisonReport(
      generator: ComparisonReportGenerator,
      src: Seq[Result],
      descriptor: SourceDescriptor
  ): Either[ReportError, Unit] = generator(src, descriptor)

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
    import scala.jdk.CollectionConverters.SeqHasAsJava
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
          .map(name => (k.receivedPoints, k.maxPoints, name))
      .sequenceRight
      .map: knowlData =>
        val plot = SpiderWebPlot(constructKnowledgeDataset(knowlData, r))
        plot.setLabelFont(getChartFont(16))
        val chart = JFreeChart(plot)
        chart.setSubtitles(List(TextTitle(parentCompetency, getChartFont(32))).asJava)
        IndividualReport.CompetencySpider(parentCompetency, chart)
  end generateCompetencyChart

  // private def getChartFont(size: Int) = Font(Font.MONOSPACED, Font.BOLD, size)
  private def getChartFont(size: Int) = Font("Helvetica", Font.BOLD, size)

  private def constructKnowledgeDataset(knowlData: Seq[(Float, Float, String)], r: Result): DefaultCategoryDataset =
    val ds = DefaultCategoryDataset()
    // if knowlData has less than 3 items, then add 2 or 1 fake item to see good SpiderWebPlot, not line or point
    if knowlData.size < 3 then
      val max = knowlData.map((_, maxPoints, _) => maxPoints).max
      for i <- 0 until knowlData.size do ds.addValue(max, r.candidate.lastname, "")
    // Add value to plot for each knowledge
    knowlData.foreach((points, _, competency) => ds.addValue(points, r.candidate.lastname, competency))
    return ds
  end constructKnowledgeDataset

  private def insertIndividualReports(
      pdf: Document,
      reports: Seq[IndividualReport],
      r: Result
  ): Either[ReportError, Unit] =
    val competencySpiderCharts = reports.collect({ case c @ IndividualReport.CompetencySpider(_, _, _) => c })
    pdf.add(getChapterParagraph("Individual competencies"))
    insertIndividualChartTable(pdf, competencySpiderCharts, r).map: _ =>
      pdf.newPage()
      ()
  end insertIndividualReports

  private def insertIndividualChartTable(
      pdf: Document,
      charts: Seq[IndividualReport.CompetencySpider],
      r: Result
  ): Either[ReportError, Unit] =
    charts
      .map(c => getPdfImage(c.chart, IndividualChartSize).map(img => c.copy(chartImg = Some(img))))
      .sequenceRight
      .map: reports =>
        pdf.add(getCandidateIndividualParagraph(r.candidate))
        pdf.add(constructTable(reports))
        ()
  end insertIndividualChartTable

  private def getCandidateIndividualParagraph(c: Interviewee): Element =
    val paragraph = Paragraph(s"${c.lastname} competencies", getPdfFont(14))
    paragraph.setAlignment(ElementTags.ALIGN_CENTER)
    return paragraph

  private def getPdfFont(size: Int) = com.lowagie.text.Font(com.lowagie.text.Font.HELVETICA, size.floatValue)

  private def getChapterParagraph(content: String): Element =
    val paragraph = Paragraph(content, getPdfFont(16))
    paragraph.setAlignment(ElementTags.ALIGN_CENTER)
    return paragraph

  private def getPdfImage(c: JFreeChart, sz: (Int, Int)): Either[ReportError, Image] =
    val img = c.createBufferedImage(1024, 1024)
    return Try(Image.getInstance(img, null)).toEither.left
      .map(_ => ReportError.GenerateError)
      .map: img =>
        img.scaleToFit(sz._1.floatValue, sz._2.floatValue)
        img
  end getPdfImage

  private def constructTable(reports: Seq[IndividualReport.CompetencySpider]): Table =
    val table = Table(2)
    table.setBorderColor(Color.WHITE)
    table.setSpacing(5f)
    reports.foreach: report =>
      val cell = Cell(report.chartImg.get)
      cell.setBorderColor(Color.WHITE)
      table.addCell(cell)
    return table

  // TODO IMPL ME
  private def comparisonGenerator(pdf: Document): ComparisonReportGenerator = (rs, s) => ???

  private enum IndividualReport:
    case CompetencySpider(competency: String, chart: JFreeChart, chartImg: Option[Image] = None)
end ReportGenerator

@main def main =
  val imported = ResultMgmtImpl.doImport(
    Vector(
      Path.of("/home/flupec/coding-projects/dcd/dcd/Avtaev-2025-05-10.json").toFile,
      Path.of("/home/flupec/coding-projects/dcd/dcd/Torbinsky-2025-05-11.json").toFile
    ),
    Path.of("/home/flupec/coding-projects/dcd/dcd/descriptor-9KsLOY.json").toFile
  )
  imported match
    case Left(value) => println(s"Import error! $value")
    case Right((results, source)) =>
      ReportGenerator.generate(results, source, fileReportTarget(Path.of("/tmp/test.pdf"))) match
        case Left(value)  => println(s"Error! Error=$value")
        case Right(value) => println("Success")
