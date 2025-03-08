import tui.withTerminal
import tui.crossterm.CrosstermJni
import tui.Terminal
import view.NumeratedListView
import view.CompetencyView
import view.KnowledgeCompleteness
import scala.collection.immutable.ArraySeq
import view.ViewController
import view.QA

val viewCtrl = ViewController(
  NumeratedListView(
    Vector(
      CompetencyView(
        "Programming",
        ArraySeq(1),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        Vector(
          QA(
            questionBody = "Definition",
            answerBody = Some(
              "Computer programming or coding is the composition of sequences of instructions, called programs, that computers can follow to perform tasks"
            )
          )
        ),
        ArraySeq.empty
      ),
      CompetencyView(
        "Variables",
        ArraySeq(1, 1),
        KnowledgeCompleteness.NotMentioned,
        ArraySeq.empty,
        Vector(
          QA(
            questionBody = "Local variables",
            answerBody = Some("A local variable is a variable that is given local scope")
          ),
          QA(
            questionBody = "Global variables",
            answerBody = Some("A global variable is a variable with global scope")
          ),
          QA(
            questionBody = "Scope of variable",
            answerBody = Some("A scope is a part of program where variable is valid")
          )
        ),
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
