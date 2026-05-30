package result

import com.lowagie.text.Anchor
import com.lowagie.text.Cell
import com.lowagie.text.Chunk
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
import common.NumerationOrdering
import common.ReportError
import common.root
import common.textView
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

  val ChapterFont = getPdfFont(24)
  val SectionFont = getPdfFont(18)
  val ParagraphFont = getPdfFont(12)

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
    val knowledgeParagraphs: Either[ReportError, IndividualReport] = generateCompetencyKnowledgesParagraph(r, s)
    val competencyCharts: Either[ReportError, Seq[IndividualReport]] = generateCompetencyCharts(r, s)
    val noteParagraphs: Either[ReportError, Seq[IndividualReport]] = generateNotes(r, s)
    val allReports = for
      kp <- knowledgeParagraphs
      np <- noteParagraphs
      cc <- competencyCharts
    yield np appendedAll cc appended kp

    for
      reports <- allReports
      _ <- insertHeader(pdf, r, s)
      _ <- insertIndividualReports(pdf, reports, r, s)
      _ <- insertPageBreak(pdf)
      _ <- insertFooter(pdf, r, s)
    yield ()
  end individualGenerator

  private def generateNotes(
      result: Result,
      source: SourceDescriptor
  ): Either[ReportError, Seq[IndividualReport]] =
    if result.noteResults.isEmpty && result.qaNoteResults.isEmpty then Right(Vector.empty)
    else
      for
        compNotes <- generateCompetencyNotes(result, source)
        qaNotes <- generateQANotes(result, source)
      yield compNotes appendedAll qaNotes

  private def generateCompetencyNotes(
      result: Result,
      source: SourceDescriptor
  ): Either[ReportError, Seq[IndividualReport]] = Right(
    for
      nr <- result.noteResults
      name <- getCompetencyName(nr.competency, result, source).toSeq
      note <- nr.notes
    yield IndividualReport.NoteParagraph(CompetencyR(name, nr.competency), None, note)
  )

  private def generateQANotes(
      result: Result,
      source: SourceDescriptor
  ): Either[ReportError, Seq[IndividualReport]] = Right(
    for
      nr <- result.qaNoteResults
      name <- getCompetencyName(nr.competency, result, source).toSeq
      note <- nr.notes
    yield IndividualReport.NoteParagraph(CompetencyR(name, nr.competency), Some(nr.qaIndex), note)
  )

  private def getCompetencyName(competency: Numeration, result: Result, source: SourceDescriptor): Option[String] =
    getCompetency(competency, result, source).map(_.name)

  private def generateCompetencyKnowledgesParagraph(
      r: Result,
      s: SourceDescriptor
  ): Either[ReportError, IndividualReport] = Right(
    IndividualReport.KnowlParagraph(
      getAllCompetencyNumerations(r, s)
        .collect(n => n -> getCompetencyKnowlPercent(r, n))
        .toMap
    )
  )

  private def getAllCompetencyNumerations(r: Result, s: SourceDescriptor): Set[Numeration] =
    s.competencies.keySet concat r.extraCompetencies.keySet

  private def getCompetencyKnowlPercent(r: Result, n: Numeration): Option[Float] =
    r.competencyResults
      .filter(cd => cd.numeration == n)
      .headOption
      .map(ckr => ckr.receivedPoints / ckr.maxPoints * 100)

  private def getCompetency(n: Numeration, r: Result, s: SourceDescriptor): Option[CompetencyDescriptor] =
    s.competencies
      .lift(n)
      .orElse(r.extraCompetencies.lift(n))

  private def generateCompetencyCharts(r: Result, s: SourceDescriptor): Either[ReportError, Seq[IndividualReport]] =
    generateCompetencyChart(r.competencyResults, r, s)
      .map(c => c.fold(Seq.empty)(Seq(_)))

  private def generateCompetencyChart(
      knowledges: Seq[CompetencyKnowledgeResult],
      r: Result,
      s: SourceDescriptor
  ): Either[ReportError, Option[IndividualReport]] =
    import scala.jdk.CollectionConverters.SeqHasAsJava
    val rootKnowledges = knowledges
      .map(_.numeration.root)
      .collect { case Some(r) => r }
      .distinct
      .flatMap(rootN => knowledges.find(_.numeration == rootN))

    val competencyKnowledge = for
      k <- rootKnowledges
      competency <- getCompetency(k.numeration, r, s)
    yield (points = k.receivedPoints, maxPoints = k.maxPoints, competency = CompetencyR(competency.name, k.numeration))

    if competencyKnowledge.isEmpty || competencyKnowledge.size < 3 then Right(None)
    else
      val plot = SpiderWebPlot(constructKnowledgeDataset(competencyKnowledge, r))
      plot.setLabelFont(getChartFont(24))
      // plot.setAxisLabelGap(-0.5d)
      val chart = JFreeChart(plot)
      chart.setSubtitles(List(TextTitle("Competencies knowledge", getChartFont(32))).asJava)
      Right(Some(IndividualReport.CompetencySpiderChart(chart)))
  end generateCompetencyChart

  private def getChartFont(size: Int) = Font("Helvetica", Font.BOLD, size)

  private def constructKnowledgeDataset(
      knowlData: Seq[(points: Float, maxPoints: Float, competency: CompetencyR)],
      r: Result
  ): DefaultCategoryDataset =
    val ds = DefaultCategoryDataset()
    // Add value to plot for each knowledge
    knowlData.foreach(d => ds.addValue(d.points, r.candidate.lastname, trimmed(d.competency.numeration.textView, 24)))
    return ds
  end constructKnowledgeDataset

  private def trimmed(src: String, maxLen: Int) = if src.length > maxLen then src.substring(0, maxLen) + "..." else src

  private def insertHeader(pdf: Document, r: Result, s: SourceDescriptor): Either[ReportError, Unit] =
    pdf.add(getChapterParagraph("Individual competencies"))
    Right(pdf.add(getCandidateIndividualParagraph(r.candidate)))

  private def insertPageBreak(pdf: Document): Either[ReportError, Unit] =
    if !pdf.newPage() then Left(ReportError.Unexpected("Cannot insert new page")) else Right(())

  private def insertIndividualReports(
      pdf: Document,
      reports: Seq[IndividualReport],
      r: Result,
      s: SourceDescriptor
  ): Either[ReportError, Unit] =
    val competencyKnowledges = reports.collect { case k: IndividualReport.KnowlParagraph => k }.head
    val notes = reports.collect { case n: IndividualReport.NoteParagraph => n }
    val competencySpiderCharts = reports.collect({ case c: IndividualReport.CompetencySpiderChart => c })
    for
      _ <- atSection(pdf, "Overview", insertCompetencyKnowledges(pdf, r, s, competencyKnowledges))
      _ <- atSection(pdf, "Notes", insertNotes(pdf, notes))
      _ <- insertIndividualChartTable(pdf, competencySpiderCharts, r)
      _ = pdf.newPage()
    yield ()
  end insertIndividualReports

  private def atSection(
      pdf: Document,
      sectionName: String,
      sectionContentWriter: => Either[ReportError, Unit]
  ): Either[ReportError, Unit] =
    pdf.add(Paragraph(sectionName, SectionFont))
    sectionContentWriter.flatMap: _ =>
      Right(pdf.add(Chunk.NEWLINE))

  private def insertCompetencyKnowledges(
      pdf: Document,
      r: Result,
      s: SourceDescriptor,
      knowl: IndividualReport.KnowlParagraph
  ): Either[ReportError, Unit] =
    val allCompetencyNumerations = knowl.knowledgePercentByCompetency.keySet.toList.sorted(using NumerationOrdering)

    val competencyListItems = for
      n <- allCompetencyNumerations
      competency <- getCompetency(n, r, s)

      percent = knowl.knowledgePercentByCompetency(n)
      listItem = createCompetencyKnowledgeListItem(n, competency.name, percent)
    yield listItem

    Right(pdf.add:
      val l = com.lowagie.text.List(false, false)
      l.setListSymbol("")
      competencyListItems.foreach(l.add(_))
      l
    )
  end insertCompetencyKnowledges

  private def createCompetencyKnowledgeListItem(
      n: Numeration,
      competencyName: String,
      knowlPercent: Option[Float]
  ): ListItem =
    // OPENPDF library obligates to construct nesting structure of ListItems for indent. We will use just text spaces
    val indent = "      ".repeat(n.size - 1)
    val text = knowlPercent match
      case Some(percent) => s"$indent${n.textView} $competencyName [${percent.toInt}%]"
      case None          => s"$indent${n.textView} $competencyName [n/a]"
    val li = ListItem(text, ParagraphFont)
    val anchor = Anchor()
    anchor.setName("competency=" + n)
    li.add(anchor)
    li
  end createCompetencyKnowledgeListItem

  /** Insert notes that was taken during report session */
  private def insertNotes(
      pdf: Document,
      notes: Seq[IndividualReport.NoteParagraph]
  ): Either[ReportError, Unit] =
    val notesByCompetency = notes.groupBy(_.competency)
    notesByCompetency.foreach: (c, notes) =>
      insertNote(pdf, c.name, c.numeration, notes.map(_.qaIndex), notes.map(_.note))
    Right(())
  end insertNotes

  private def insertNote(
      pdf: Document,
      competency: String,
      competencyN: Numeration,
      qaIndices: Seq[Option[Int]],
      notes: Seq[String]
  ): Either[ReportError, Unit] =
    if qaIndices.size != notes.size then return Left(ReportError.Unexpected("qaIndices size not equal to notes size"))

    val noteItems = for
      i <- 0 until notes.size
      note = notes(i)
      qaIndex = qaIndices(i)
      listItem =
        val li = ListItem()
        li.setFont(ParagraphFont)
        qaIndex match
          case Some(qaIndex) =>
            val anchor = Anchor(s"Q${qaIndex + 1} ", ParagraphFont)
            anchor.setReference(qaRelativeLink(competencyN, qaIndex))
            li.add(anchor)
          case None => ()
        li.add(note)
        li
      _ = listItem.setIndentationLeft(16f)
    yield listItem

    val notesList =
      val l = com.lowagie.text.List(false, false)
      l.setListSymbol("• ")
      l.setAlignindent(true)
      l.setIndentationLeft(16f)
      noteItems.foreach(l.add(_))
      l

    pdf.add(Paragraph(competency, ParagraphFont))
    pdf.add(notesList)
    Right(())
  end insertNote

  /** Inserts a chart that shows different competency scores */
  private def insertIndividualChartTable(
      pdf: Document,
      charts: Seq[IndividualReport.CompetencySpiderChart],
      r: Result
  ): Either[ReportError, Unit] =
    charts
      .map(c => getPdfImage(c.chart, IndividualChartSize).map(img => c.copy(chartImg = Some(img))))
      .sequenceRight
      .map: reports =>
        pdf.add(constructTable(reports))
        ()
  end insertIndividualChartTable

  private def insertFooter(pdf: Document, r: Result, s: SourceDescriptor): Either[ReportError, Unit] =
    insertQAInfo(pdf, s)

  private def insertQAInfo(pdf: Document, s: SourceDescriptor): Either[ReportError, Unit] =
    val allCompetencyNumerations = s.competencies.keys.toList.sorted(using NumerationOrdering)
    for
      n <- allCompetencyNumerations
      competency <- s.competencies.lift(n)
      qaListItems = competency.qa.zipWithIndex.map((qa, idx) => createQAListItem(qa, idx, n))
      _ = pdf.add:
        val l = com.lowagie.text.List(false, false)
        qaListItems.foreach: li =>
          pdf.add(li)
          pdf.add(Chunk.NEWLINE)
        l
    yield ()
    Right(())
  end insertQAInfo

  private def createQAListItem(qa: QADescriptor, index: Int, n: Numeration): ListItem =
    val anchor = Anchor()
    anchor.setFont(ParagraphFont)
    anchor.setName(qaAnchorName(n, index))
    anchor.add(s"[${n.textView}] Q${index + 1}: ${qa.question}")
    anchor.add(Chunk.NEWLINE)
    qa.answer.foreach(ans => anchor.add(s"Ans: $ans"))
    val li = ListItem()
    li.setFont(ParagraphFont)
    li.add(anchor)
    li
  end createQAListItem

  private def qaRelativeLink(competency: Numeration, qaIndex: Int) = "#" + qaAnchorName(competency, qaIndex)

  private def qaAnchorName(competency: Numeration, qaIndex: Int) = s"competency=${competency.textView}&idx=$qaIndex"

  private def getCandidateIndividualParagraph(c: Interviewee): Element =
    val paragraph = Paragraph(s"${c.lastname} competencies", ParagraphFont)
    paragraph.setAlignment(ElementTags.ALIGN_CENTER)
    return paragraph

  private def getPdfFont(size: Int) = com.lowagie.text.Font(com.lowagie.text.Font.HELVETICA, size.floatValue)

  private def getChapterParagraph(content: String): Element =
    val paragraph = Paragraph(content, ChapterFont)
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

  private def constructTable(reports: Seq[IndividualReport.CompetencySpiderChart]): Table =
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
    case KnowlParagraph(knowledgePercentByCompetency: Map[Numeration, Option[Float]])

    /** Graph that shows knowledge for different competencies
      */
    case CompetencySpiderChart(chart: JFreeChart, chartImg: Option[Image] = None)

    /** Text information with notes from estimate session
      */
    case NoteParagraph(competency: CompetencyR, qaIndex: Option[Int], note: String)
  end IndividualReport

  private case class CompetencyR(name: String, numeration: Numeration)
end ReportGenerator
