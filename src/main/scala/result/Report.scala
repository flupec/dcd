package result

import com.lowagie.text.Cell
import com.lowagie.text.Document
import com.lowagie.text.Element
import com.lowagie.text.ElementTags
import com.lowagie.text.Image
import com.lowagie.text.ListItem
import com.lowagie.text.Paragraph
import com.lowagie.text.Table
import com.lowagie.text.pdf.PdfWriter
import common.EitherExtension.sequenceRight
import common.Numeration
import common.ReportError
import common.directParent
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.SpiderWebPlot
import org.jfree.chart.title.TextTitle
import org.jfree.data.category.DefaultCategoryDataset

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
      case Failure(err) => Left(ReportError.Unexpected(err.getMessage))
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
  // private def generateComparisonReport(
  //     generator: ComparisonReportGenerator,
  //     src: Seq[Result],
  //     descriptor: SourceDescriptor
  // ): Either[ReportError, Unit] = generator(src, descriptor)

  private def individualGenerator(pdf: Document): IndividualReportGenerator = (r, s) =>
    val competencyCharts: Either[ReportError, Seq[IndividualReport]] = generateCompetencyCharts(r, s)
    val noteParagraphs: Either[ReportError, Seq[IndividualReport]] = generateCompetencyNotes(r, s)
    val allReports = for
      np <- noteParagraphs
      cc <- competencyCharts
    yield np appendedAll cc

    for reports <- allReports yield insertIndividualReports(pdf, reports, r)
  end individualGenerator

  private def generateCompetencyNotes(
      result: Result,
      source: SourceDescriptor
  ): Either[ReportError, Seq[IndividualReport.NotesParagraph]] =
    result.noteResults
      .map(nr => getCompetencyName(nr.competency, source).map(name => nameAtEachNote(nr, name)))
      .sequenceRight match
      case Left(err)    => Left(err)
      case Right(notes) => Right(notes.map(n => IndividualReport.NotesParagraph(n)))

  private def nameAtEachNote(nr: CompetencyNoteResult, competecyName: String): Seq[(competency: String, note: String)] =
    nr.notes.map(note => (competecyName, note))

  private def getCompetencyName(competency: Numeration, source: SourceDescriptor): Either[ReportError, String] =
    source.competencies
      .get(competency) match
      case None             => Left(ReportError.InconsistentSourceDescriptor(s"Not found competency ${competency}"))
      case Some(descriptor) => Right(descriptor.name)

  private def generateCompetencyCharts(r: Result, s: SourceDescriptor): Either[ReportError, Seq[IndividualReport]] =
    val resultsByNestLevel = r.competencyResults.groupBy(_.numeration.size)
    resultsByNestLevel.values
      .map(generateCompetencyChart(_, r, s))
      .toSeq
      .sequenceRight

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
          .fold(Left(ReportError.notFoundNumeration(k.numeration)))(Right(_))
          .map(name => (k.receivedPoints, k.maxPoints, name))
      .sequenceRight
      .map: knowlData =>
        val plot = SpiderWebPlot(constructKnowledgeDataset(knowlData, r))
        plot.setLabelFont(getChartFont(18))
        plot.setAxisLabelGap(-0.5d)
        val chart = JFreeChart(plot)
        chart.setSubtitles(List(TextTitle(parentCompetency, getChartFont(32))).asJava)
        IndividualReport.CompetencySpider(parentCompetency, chart)
  end generateCompetencyChart

  private def getChartFont(size: Int) = Font("Helvetica", Font.BOLD, size)

  private def constructKnowledgeDataset(knowlData: Seq[(Float, Float, String)], r: Result): DefaultCategoryDataset =
    val ds = DefaultCategoryDataset()
    // if knowlData has less than 3 items, then add 2 or 1 fake item to see good SpiderWebPlot, not line or point
    if knowlData.size < 3 then
      val max = knowlData.map((_, maxPoints, _) => maxPoints).max
      for i <- 0 until knowlData.size do ds.addValue(max, r.candidate.lastname, "")
    // Add value to plot for each knowledge
    knowlData.foreach((points, _, competency) => ds.addValue(points, r.candidate.lastname, trimmed(competency, 24)))
    return ds
  end constructKnowledgeDataset

  private def trimmed(src: String, maxLen: Int) = if src.length > maxLen then src.substring(0, maxLen) + "..." else src

  private def insertIndividualReports(
      pdf: Document,
      reports: Seq[IndividualReport],
      r: Result
  ): Either[ReportError, Unit] =
    pdf.add(getChapterParagraph("Individual competencies"))
    pdf.add(getCandidateIndividualParagraph(r.candidate))
    val competencyNotes = reports.collect { case IndividualReport.NotesParagraph(notes) => notes }.toSeq.flatten
    val competencySpiderCharts = reports.collect({ case c @ IndividualReport.CompetencySpider(_, _, _) => c })
    for
      _ <- insertNotes(pdf, competencyNotes)
      _ <- insertIndividualChartTable(pdf, competencySpiderCharts, r)
      _ = pdf.newPage()
    yield ()
  end insertIndividualReports

  private def insertNotes(pdf: Document, notes: Seq[(competency: String, note: String)]): Either[ReportError, Unit] =
    val notesByName =
      for (name, notesUnflatten) <- notes.groupBy(_.competency)
      yield (name, notesUnflatten.map(_.note))
    notesByName
      .map((name, notes) => insertNote(pdf, name, notes))
      .toSeq
      .sequenceRight
      .map(_ => ())
  end insertNotes

  private def insertNote(pdf: Document, competency: String, notes: Seq[String]): Either[ReportError, Unit] =
    val font = getPdfFont(12)
    val competencyLine = Paragraph(competency, font)
    val noteItems = for
      note <- notes
      listItem = ListItem(note, font)
      _ = listItem.setIndentationLeft(16f)
    yield listItem

    val notesList =
      val l = com.lowagie.text.List(false, false)
      l.setListSymbol("• ")
      l.setAlignindent(true)
      l.setIndentationLeft(16f)
      noteItems.foreach(l.add(_))
      l

    pdf.add(competencyLine)
    pdf.add(notesList)
    Right(())
  end insertNote

  private def insertIndividualChartTable(
      pdf: Document,
      charts: Seq[IndividualReport.CompetencySpider],
      r: Result
  ): Either[ReportError, Unit] =
    charts
      .map(c => getPdfImage(c.chart, IndividualChartSize).map(img => c.copy(chartImg = Some(img))))
      .sequenceRight
      .map: reports =>
        pdf.add(constructTable(reports))
        ()
  end insertIndividualChartTable

  private def getCandidateIndividualParagraph(c: Interviewee): Element =
    val paragraph = Paragraph(s"${c.lastname} competencies", getPdfFont(14))
    paragraph.setAlignment(ElementTags.ALIGN_CENTER)
    return paragraph

  private def getPdfFont(size: Int) = com.lowagie.text.Font(com.lowagie.text.Font.HELVETICA, size.floatValue)

  private def getChapterParagraph(content: String): Element =
    val paragraph = Paragraph(content, getPdfFont(18))
    paragraph.setAlignment(ElementTags.ALIGN_CENTER)
    return paragraph

  private def getPdfImage(c: JFreeChart, sz: (Int, Int)): Either[ReportError, Image] =
    val img = c.createBufferedImage(1000, 1000)
    return Try(Image.getInstance(img, null)).toEither.left
      .map(e => ReportError.Unexpected(e.getMessage))
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
  // private def comparisonGenerator(pdf: Document): ComparisonReportGenerator = (rs, s) => ???

  private enum IndividualReport:
    case CompetencySpider(competency: String, chart: JFreeChart, chartImg: Option[Image] = None)
    case NotesParagraph(notes: Seq[(competency: String, note: String)])
end ReportGenerator
