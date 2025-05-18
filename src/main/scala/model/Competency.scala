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

  override def hashCode: Int = numeration.hashCode

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

  def flatten: Competencies =
    val childs = if this.childs.nonEmpty then this.childs.flatMap(_.flatten) else Seq.empty
    childs :+ this

  def hashTarget: String = this.flatten
    .sortBy(_.numeration)(using NumerationOrdering)
    .map(c => s"num=${c.numeration},name=${c.name},qa=${qa.map(_.hashTarget)}")
    .mkString

end Competency

case class QA(
    question: String,
    answer: Option[String],
    knowledgeTest: KnowledgeTest = KnowledgeTest()
):
  def withKnowledge(k: KnowledgeTest) = copy(knowledgeTest = k)

  def hashTarget: String = s"q=$question,a=$answer,mp=${knowledgeTest.maxPoints}"
end QA

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

// Meaningful only for competencies, questions knowledge provided as-is since there are no tree structure in questions
class KnowledgeComputed(
    // Numeration of competency knowledge
    val numeration: Numeration,
    // Maximum points possible points for this competency (sum of childs maxPoints and this competency qa maxPoints)
    val maxPoints: Float,
    // Computed received points
    val receivedPoints: Float,
    // If computation overriden by parent, then this will be non empty
    val overridenBy: Option[KnowledgeComputed] = None,
    // If true, then received points computed only from child competencies. False for estimated competencies or it's qa
    // plus child competencies
    val synthetic: Boolean = false
) derives CanEqual:
  require(maxPoints > 0f, "maxPoints less or equal to zero")
  require(receivedPoints >= 0f, "received points less zero")

  override def equals(that: Any): Boolean = that match
    case that: KnowledgeComputed => that.numeration == numeration
    case _                       => false

  override def hashCode(): Int = numeration.hashCode()

  def copy(
      numeration: Numeration = numeration,
      overridenBy: Option[KnowledgeComputed] = overridenBy,
      synthetic: Boolean = synthetic
  ) = KnowledgeComputed(numeration, maxPoints, receivedPoints, overridenBy, synthetic)

  /** Maintains invariant:
    *    if `a` overrides `b` and `b` overrides `this`, then `a` overrides `this`.
    *    And we must give preference for `overridenBy` field to highest ancestor (`a` in the example above)
    */
  def markOverriden(by: KnowledgeComputed): KnowledgeComputed = overridenBy match
    case None                                                                     => copy(overridenBy = Some(by))
    case Some(currOverriden) if currOverriden.numeration.isChildOf(by.numeration) => copy(overridenBy = Some(by))
    case Some(currOVerriden)                                                      => this

  def asSynthetic: KnowledgeComputed = copy(synthetic = true)
end KnowledgeComputed

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
end KnowledgeComputed
