import controller.CompetenciesControllerImpl
import model.Competency
import model.QA
import tui.Terminal
import tui.crossterm.CrosstermJni
import tui.withTerminal
import view.CompetenciesView
import view.MessageShow
import view.LogView

val Controller = CompetenciesControllerImpl(
  Vector(
    Competency(
      numeration = Vector(1),
      name = "Programming",
      qa = Vector(
        QA(
          question = "Definition",
          answer = Some(
            "Computer programming or coding is the composition of sequences of instructions, called programs, that computers can follow to perform tasks"
          )
        )
      ),
      childs = Vector(
        Competency(
          numeration = Vector(1, 1),
          name = "Variables",
          qa = Vector(
            QA(
              question = "Local variables",
              answer = Some("A local variable is a variable that is given local scope")
            ),
            QA(
              question = "Global variables",
              answer = Some("A global variable is a variable with global scope")
            ),
            QA(
              question = "Scope of variable",
              answer = Some("A scope is a part of program where variable is valid")
            )
          ),
          childs = Vector.empty
        ),
        Competency(
          numeration = Vector(1, 2),
          name = "Control structures",
          qa = Vector.empty,
          childs = Vector(
            Competency(
              numeration = Vector(1, 2, 1),
              name = "While loop",
              qa = Vector.empty,
              childs = Vector.empty
            )
          )
        )
      )
    ),
    Competency(
      numeration = Vector(2),
      name = "Java",
      qa = Vector.empty,
      childs = Vector(
        Competency(
          numeration = Vector(2, 1),
          name = "Exception hierarchy",
          qa = Vector.empty,
          childs = Vector.empty
        ),
        Competency(
          numeration = Vector(2, 2),
          name = "Collections API",
          qa = Vector.empty,
          childs = Vector.empty
        )
      )
    )
  )
)

val LogBar = LogView()

given MessageShow = LogBar.updateLogs

val NumListView = CompetenciesView(Controller)
val ViewController = view.ViewController(NumListView, LogBar)

@main def hello(): Unit =
  withTerminal(appLoop)

private def appLoop(jni: CrosstermJni, terminal: Terminal) =
  while true do
    ViewController.handleInput(jni)
    terminal.draw(frame => ViewController.render(frame, frame.size))
