import common.CliError
import controller.CompetenciesControllerImpl
import model.readCompetencies
import result.Interviewee
import result.ResultMgmtImpl
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

@main def main(args: String*): Unit =
  if args.isEmpty then
    println("Usage: dcd -f ./example.dcd -l Smith")
    return

  val startResult: Either[common.Error, Unit] = for
    competenciesFilepath: Path <- getCliArg("-f", args.toList)
    candidateLastname: String <- getCliArg("-l", args.toList)
    competencies <- readCompetencies(competenciesFilepath.toFile)
    resultMgmt = ResultMgmtImpl(competenciesFilepath, Interviewee(candidateLastname), result.fsExportTgtLocator)
    controller = CompetenciesControllerImpl(resultMgmt, competencies)
    competenciesView = CompetenciesView(controller)
    viewController = view.ViewController(competenciesView, LogBar)
  yield withTerminal(appLoop(viewController))

  if startResult.isLeft then
    println(startResult.left.get.reason)
    return

private def getCliArg[A](key: String, args: List[String])(using
    argParser: CommandLineParser.FromString[A]
): Either[common.Error, A] =
  val foundArgValue: Option[A] = args match
    case k :: value :: _ if key == k => argParser.fromStringOption(value)
    case _ :: _ :: rest              => getCliArg(key, rest).toOption
    case _                           => None
  Either.cond(foundArgValue.isDefined, foundArgValue.get, CliError.MissedMandatoryArg(key))

given CommandLineParser.FromString[Path] with
  def fromString(s: String): Path = Path.of(s)

given CommandLineParser.FromString[String] with
  def fromString(s: String): String = s

private def appLoop(viewCntrl: ViewController)(jni: CrosstermJni, terminal: Terminal): Unit =
  while true do
    val mustContinue = viewCntrl.handleInput(jni)
    if mustContinue then terminal.draw(frame => viewCntrl.render(frame, frame.size))
    else return
