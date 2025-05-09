package result

import com.lowagie.text.Cell
import com.lowagie.text.ChapterAutoNumber
import com.lowagie.text.Document
import com.lowagie.text.Image
import com.lowagie.text.Table
import com.lowagie.text.pdf.PdfWriter
import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.SpiderWebPlot
import org.jfree.data.category.DefaultCategoryDataset

import java.awt.Color
import java.io.FileOutputStream
import java.nio.file.Path

@main def main =
  val ds = DefaultCategoryDataset()
  ds.addValue(95, "Avtaev", "Programming")
  ds.addValue(75, "Avtaev", "Social skills")
  ds.addValue(40, "Avtaev", "Network")

  ds.addValue(40, "Smith", "Programming")
  ds.addValue(50, "Smith", "Social skills")
  ds.addValue(60, "Smith", "Network")

  val plt = SpiderWebPlot(ds)
  plt.setMaxValue(100f)
  val chart = JFreeChart(plt)

  // ChartUtils.saveChartAsPNG(Path.of("/tmp", "test.png").toFile, chart, 1280, 720)

  val img = chart.createBufferedImage(480, 320)

  val pdf = Document()
  PdfWriter.getInstance(pdf, FileOutputStream(Path.of("/tmp/test.pdf").toFile))
  pdf.open()

  val imagesTable = Table(2)
  imagesTable.setBorderColor(Color.WHITE)
  imagesTable.setSpacing(5f)
  imagesTable.addCell(Cell(Image.getInstance(img, null)))
  imagesTable.addCell(Cell(Image.getInstance(img, null)))
  pdf.add(ChapterAutoNumber("First comparison"))
  pdf.add(imagesTable)
  pdf.add(ChapterAutoNumber("Second comparison"))
  pdf.add(imagesTable)

  pdf.close()
