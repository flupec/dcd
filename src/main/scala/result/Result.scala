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
    hash: String
)

object SourceDescriptor:
  given ReadWriter[SourceDescriptor] = macroRW

case class CompetencyDescriptor(
    name: String,
    qa: Seq[QADescriptor]
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
    competencyResults: Seq[CompetencyKnowledgeResult],
    qaResults: Seq[QAKnowledgeResult],
    noteResults: Seq[CompetencyNoteResult]
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
    numeration: Numeration,
    // Maximum possible points for competency
    maxPoints: Float,
    // Points received by interviewee
    receivedPoints: Float
)

object CompetencyKnowledgeResult:
  given ReadWriter[CompetencyKnowledgeResult] = macroRW

// Question knowledge estimation result
case class QAKnowledgeResult(
    // Competency id
    competency: Numeration,
    // Index of qa in competency
    idx: Int,
    // Estimation of this question
    estimate: Int
)

object QAKnowledgeResult:
  given ReadWriter[QAKnowledgeResult] = macroRW

// Created notes during interview
case class CompetencyNoteResult(
    // Competency id
    competency: Numeration,
    // Notes for this competency
    notes: Seq[String]
)

object CompetencyNoteResult:
  given ReadWriter[CompetencyNoteResult] = macroRW