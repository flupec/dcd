package model

import common.*

type Competencies = Seq[Competency]

class Competency(
    val numeration: Numeration,
    val name: String,
    val qa: Seq[QA],
    val childs: Competencies,
    // Represents knowledge estimate that created from user input
    val estimate: KnowledgeEstimate = KnowledgeEstimate.NotMentioned,
    val notes: Seq[String] = Seq.empty
) derives CanEqual:
  override def equals(that: Any): Boolean = that match
    case that: Competency => numeration == that.numeration
    case _                => false

  override def hashCode(): Int = numeration.hashCode()

  def copy(
      childs: Competencies = childs,
      estimate: KnowledgeEstimate = estimate,
      qa: Seq[QA] = qa
  ) = Competency(
    numeration = numeration,
    name = name,
    qa = qa,
    childs = childs,
    estimate = estimate
  )

case class QA(
    question: String,
    answer: Option[String],
    knowledgeTest: KnowledgeTest = KnowledgeTest()
):
  def withKnowledge(k: KnowledgeTest) = copy(knowledgeTest = k)

case class KnowledgeTest(
    // How many maximum points we can get for this knowledge
    maxPoints: Float = 1f,
    // Estimation of knowledge
    estimate: KnowledgeEstimate = KnowledgeEstimate.NotMentioned
) derives CanEqual:
  require(maxPoints > 0f)

enum KnowledgeEstimate derives CanEqual:
  // Knowledge is abandoned on computation and not taken into account
  case NotMentioned
  // Will be used at knowledge computation
  case Answered(val percent: Int)

class KnowledgeComputed(
    // Numeration of competency knowledge
    val numeration: Numeration,
    // Maximum points possible points for this competency (sum of childs maxPoints and this competency qa maxPoints)
    val maxPoints: Float,
    // Actually received points
    val receivedPoints: Float
) derives CanEqual:
  require(maxPoints > 0f, "maxPoints less or equal to zero")
  require(receivedPoints >= 0f, "received points less zero")

  override def equals(that: Any): Boolean = that match
    case that: KnowledgeComputed => that.numeration == numeration
    case _                       => false

  override def hashCode(): Int = numeration.hashCode()

  def copy(numeration: Numeration = numeration) = KnowledgeComputed(numeration, maxPoints, receivedPoints)

object KnowledgeComputed:
  /** Sum knowledges and sets numeration for summary knowledge */
  def summarizer(outNumeration: Numeration)(l: KnowledgeComputed, r: KnowledgeComputed) =
    sum(l, r).copy(numeration = outNumeration)

  /** Sums knowledge */
  private def sum(l: KnowledgeComputed, r: KnowledgeComputed) = KnowledgeComputed(
    numeration = l.numeration,
    maxPoints = l.maxPoints + r.maxPoints,
    receivedPoints = l.receivedPoints + r.receivedPoints
  )

  /** Returns computed knowledge with maxPoints = 1, returns none if estimate not */
  def fromCompetencyEstimate(c: Competency): Option[KnowledgeComputed] = c.estimate match
    case KnowledgeEstimate.NotMentioned => None
    case KnowledgeEstimate.Answered(percent) =>
      Some(KnowledgeComputed(numeration = c.numeration, maxPoints = 1f, receivedPoints = percent / 100f))
