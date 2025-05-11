import common.CliError
import controller.CompetenciesControllerImpl
import model.readCompetencies
import result.Interviewee
import result.ReportGenerator
import result.ReportGenerator.fileReportTarget
import result.ResultExporter
import result.ResultImporter
import tui.Terminal
import tui.crossterm.CrosstermJni
import tui.withTerminal
import view.CompetenciesView
import view.LogView
import view.MessageShow
import view.ViewController

import java.nio.file.Path
import scala.util.CommandLineParser

val LogBar = LogView()
given MessageShow = LogBar.updateLogs

val Help = """Commands:
  estimate - starts tui session to estimate candidate competencies
  report - generate report from previously estimated competency knowledges

Usage:
  dcd estimate -f <competency-src-file> -l <candidate-lastname>
  dcd report -s <src-descriptor-file> -t <dest-file> -r <result-file> -r <result-file>...

Examples:
  dcd estimate -f ./example.dcd -l Smith
  dcd report -s ./descriptor-9KsLOY.json -r ./Smith-2025-05-10.json -t /tmp/report.pdf"""

@main def main(args: String*): Unit =
  if args.isEmpty then
    println(Help)
    return

  val argNoCommand = args.toList.tail

  return args(0) match
    case "estimate" => estimateCommand(argNoCommand)
    case "report"   => reportCommand(argNoCommand)
    case cmd =>
      println(s"Unknown command '$cmd'")
      println(Help)
end main

private def estimateCommand(args: List[String]): Unit =
  val startResult: Either[common.Error, Unit] = for
    competenciesFilepath: Path <- getMandatoryCliOption("-f", args)
    candidateLastname: String <- getMandatoryCliOption("-l", args)
    competencies <- readCompetencies(competenciesFilepath.toFile)
    resultMgmt = ResultExporter(competencies, Interviewee(candidateLastname))
    controller = CompetenciesControllerImpl(resultMgmt, competencies)
    competenciesView = CompetenciesView(controller)
    viewController = view.ViewController(competenciesView, LogBar)
  yield withTerminal(appLoop(viewController))

  if startResult.isLeft then
    println(startResult.left.get.reason)
    println(Help)
    return
end estimateCommand

/** Returns single cli option value */
private def getMandatoryCliOption[A](key: String, args: List[String])(using
    argParser: CommandLineParser.FromString[A]
): Either[common.Error, A] = getMandatoryCliOptions(key, args, List()).map(_.head)

/** Returns cli option values for repetitive option key */
private def getMandatoryCliOptions[A](key: String, args: List[String])(using
    argParser: CommandLineParser.FromString[A]
): Either[common.Error, List[A]] =
  getMandatoryCliOptions(key, args, List())

private def getMandatoryCliOptions[A](key: String, args: List[String], found: List[A])(using
    argParser: CommandLineParser.FromString[A]
): Either[common.Error, List[A]] =
  val argValues: Either[common.Error, List[A]] = args match
    // Key match, try to parse value and lookup further
    case k :: value :: rest if key == k =>
      val parsedValue = argParser.fromStringOption(value)
      if parsedValue.isEmpty then return Left(CliError.BadArgumentFormat(key))
      else getMandatoryCliOptions(key, rest, parsedValue.get :: found)

    // Key not matches, try to lookup further
    case _ :: _ :: rest => getMandatoryCliOptions(key, rest, found)

    // All cli options traversed, return accumulated found arg values
    case _ => Right(found)
  return argValues.filterOrElse(_.nonEmpty, CliError.MissedMandatoryArg(key))
end getMandatoryCliOptions

private def reportCommand(args: List[String]): Unit =
  val startResult: Either[common.Error, Unit] = for
    sourceDescriptorFilepath: Path <- getMandatoryCliOption("-s", args)
    resultsFilepath: List[Path] <- getMandatoryCliOptions("-r", args)
    targetFilepath: Path <- getMandatoryCliOption("-t", args)
    (results, descriptor) <- ResultImporter.doImport(resultsFilepath.map(_.toFile), sourceDescriptorFilepath.toFile)
    _ <- ReportGenerator.generate(results, descriptor, fileReportTarget(targetFilepath))
  yield ()

  if startResult.isLeft then
    println(startResult.left.get.reason)
    println(Help)
    return
end reportCommand

given CommandLineParser.FromString[Path] with
  def fromString(s: String): Path = Path.of(s)

given CommandLineParser.FromString[String] with
  def fromString(s: String): String = s

private def appLoop(viewCntrl: ViewController)(jni: CrosstermJni, terminal: Terminal): Unit =
  while true do
    val mustContinue = viewCntrl.handleInput(jni)
    if mustContinue then terminal.draw(frame => viewCntrl.render(frame, frame.size))
    else return
