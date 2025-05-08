package result

import common.Numeration

import upickle.default.ReadWriter
import upickle.default.macroRW

/** Source file descriptor. Contains all valuable information of source file at exporte functionality
  *
  * @param competencies competency descriptions
  * @param hash source file hash
  */
case class SourceDescriptor(
    competencies: Map[Numeration, CompetencyDescriptor],
    hash: String,
)

object SourceDescriptor:
  given ReadWriter[SourceDescriptor] = macroRW

case class CompetencyDescriptor(
    name: String,
    qa: Seq[QADescriptor],
    notes: Seq[String]
)

object CompetencyDescriptor:
  given ReadWriter[CompetencyDescriptor] = macroRW

case class QADescriptor(
    question: String,
    answer: Option[String]
)

object QADescriptor:
  given ReadWriter[QADescriptor] = macroRW

// Interview results
case class Result(
    sourceDescriptorHash: String,
    candidate: Interviewee,
    competencyResults: Seq[CompetencyKnowledgeResult]
)

object Result:
  given ReadWriter[Result] = macroRW

// Who was interviewed
case class Interviewee(
    val lastname: String
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
    val receivedPoints: Float
)

object CompetencyKnowledgeResult:
  given ReadWriter[CompetencyKnowledgeResult] = macroRW
