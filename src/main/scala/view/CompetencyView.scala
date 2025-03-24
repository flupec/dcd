package view

import common.*

class CompetencyView(
    val name: String,
    val numeration: Numeration,
    val completeness: KnowledgeCompleteness = KnowledgeCompleteness.NotMentioned,
    val questions: Seq[QA],
    val notes: Seq[String] = Seq.empty
):
  override def toString: String = s"{numeration=$numeration, name=$name, completeness=$completeness}"

  override def equals(that: Any): Boolean = that match
    case that: CompetencyView => numeration == that.numeration
    case _                    => false

  override def hashCode(): Int = numeration.hashCode

  def numerationView: String = numeration
    .map(_.toString)
    .reduceLeft((left, right) => s"${left.toString}.${right.toString}")

case class QA(
    questionBody: String,
    status: KnowledgeCompleteness = KnowledgeCompleteness.NotMentioned,
    answerBody: Option[String] = Option.empty
)

enum KnowledgeCompleteness derives CanEqual:
  case NotMentioned
  case Answered(val percent: Int)
  case Unanswered

case class KnowledgeComputed(percent: Int, overridenBy: Option[Numeration], synthetic: Boolean)