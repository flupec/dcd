import tui.withTerminal
import tui.crossterm.CrosstermJni
import tui.Terminal
import tui.crossterm.Duration
import tui.crossterm.Event
import tui.crossterm.KeyCode
import view.NumeratedListView
import view.CompetencyView
import view.KnowledgeCompleteness
import scala.collection.immutable.ArraySeq
import view.ViewController

val viewCtrl = ViewController(
  NumeratedListView(
    Vector(
      CompetencyView(
        "Programming",
        ArraySeq(1),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      ),
      CompetencyView(
        "Variables",
        ArraySeq(1, 1),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      ),
      CompetencyView(
        "Control structures",
        ArraySeq(1, 2),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      ),
      CompetencyView(
        "While loop",
        ArraySeq(1, 2, 1),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      ),
      CompetencyView(
        "Java",
        ArraySeq(2),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      ),
      CompetencyView(
        "Exception hierarchy",
        ArraySeq(2, 1),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      ),
      CompetencyView(
        "Collections API",
        ArraySeq(2, 2),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        ArraySeq.empty,
        ArraySeq.empty
      )
    )
  )
)

@main def hello(): Unit =
  withTerminal(appLoop)

private def appLoop(jni: CrosstermJni, terminal: Terminal) =
  while true do
    viewCtrl.handleInput(jni)
    terminal.draw(frame => viewCtrl.render(frame, frame.size))

private def input(jni: CrosstermJni) =
  if jni.poll(Duration(0, 1000)) then
    jni.read() match
      case key: Event.Key => handleKeyInput(key)
      case _              => ()

private def handleKeyInput(key: Event.Key) =
  key.keyEvent.code match
    case char: KeyCode.Char if char.c() == 'q' => System.exit(0)
    case _                                     => ()
