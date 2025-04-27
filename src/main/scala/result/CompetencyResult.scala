package result

import common.Numeration

import upickle.default.ReadWriter
import upickle.default.macroRW

// Interview results
case class Result(
    sourceFilename: String,
    candidate: Interviewee,
    competencyResults: Seq[CompetencyKnowledgeResult]
)

object Result:
  given ReadWriter[Result] = macroRW    

// Who was interviewed
case class Interviewee(
    val lastname: String,
)

object Interviewee:
  given ReadWriter[Interviewee] = macroRW

// Knowledge results
case class CompetencyKnowledgeResult(
    // Competency numeration
    val numeration: Numeration,
    // Maximum possible points for competency
    val maxPoints: Float,
    // Points received by interviewee
    val receivedPoints: Float,
)

object CompetencyKnowledgeResult:
  given ReadWriter[CompetencyKnowledgeResult] = macroRW
