import common.CliError
import controller.CompetenciesControllerImpl
import model.readCompetencies
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

@main def hello(args: String*): Unit =
  if args.isEmpty then
    println("Usage: dcd -f ./example.dcd")
    return

  val startResult: Either[common.Error, Unit] = for
    competenciesFilepath <- getCliArg("-f", args.toList)
    competencies <- readCompetencies(competenciesFilepath.toFile)
    controller = CompetenciesControllerImpl(competencies)
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
    case key :: value :: _ => argParser.fromStringOption(value)
    case _                 => None
  Either.cond(foundArgValue.isDefined, foundArgValue.get, CliError.MissedMandatoryArg(key))

given CommandLineParser.FromString[Path] with
  def fromString(s: String): Path = Path.of(s)

private def appLoop(viewCntrl: ViewController)(jni: CrosstermJni, terminal: Terminal) =
  while true do
    viewCntrl.handleInput(jni)
    terminal.draw(frame => viewCntrl.render(frame, frame.size))
