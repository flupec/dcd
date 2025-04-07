package model

import common.Numeration
import common.NumerationOrdering
import common.directParent
import common.isChildOf
import common.isDirectParentOf
import common.next

type Updater = (Competency) => Competency
type Filter = (Competency) => Boolean

/** Returns filter that filters by competency numeration */
def byNumeration(n: Numeration): Filter = (c) => c.numeration == n

/** Returns updater that updates competency estimate */
def estimateUpdater(e: KnowledgeEstimate): Updater = (c) => c.copy(estimate = e)

/** Returns updater that updates knowledge for qa at specified index
  *
  * @param t new knowledge test
  * @param idx index of qa item in competency
  * @return updater function
  */
def qaKnowledgeUpdater(t: KnowledgeTest, idx: Int): Updater = (competency) =>
  val qa = competency.qa
  val updatedQa = if idx > qa.size - 1 then qa else qa.updated(idx, qa(idx).withKnowledge(t))
  competency.copy(qa = updatedQa)

/** Updated list of competencies
  *
  * @param cs competencies list
  * @param filter criteria
  * @param upd update action
  * @return updated competencies list
  */
def updateCompetencies(cs: Competencies, filter: Filter, upd: Updater): Competencies =
  for i <- 0 until cs.size yield
    val updCandidate = cs(i)
    updateCompetency(updCandidate, filter, upd) match
      case None          => updCandidate
      case Some(updated) => updated

private def updateCompetency(c: Competency, filter: Filter, upd: Updater): Option[Competency] =
  if filter(c) then Some(upd(c))
  else
    val updatedChilds: Seq[Option[Competency]] = c.childs.map(updateCompetency(_, filter, upd))
    updatedChilds.indexWhere(_.isDefined) match
      case -1 => None
      case idx =>
        val updated = updatedChilds(idx).get
        Some(c.copy(childs = c.childs.updated(idx, updated)))

/** Computes knowledges of given competency. Each competency knowledge is sum of parent knowledge and child knowledge
  *  unless parent knowledge competency estimate not set
  *
  * @param c competency tree root
  * @return Map of computed knowledges for each competency in the tree
  */
def computeKnowledge(c: Competency): Map[Numeration, KnowledgeComputed] =
  val sub = if c.childs.isEmpty then Map.empty else c.childs.flatMap(computeKnowledge(_)).toMap
  val competencyEstim = KnowledgeComputed.fromCompetencyEstimate(c)
  val qaKnowl = computeQAKnowledge(c)

  // If formed by QA knowledge or competency not estimated, then we must sum curr knowledge with sub knowledges
  // Otherwise, force set curr knowledge (ignore child and qa knowledges)
  val (currKnowledge, forceSet) = (competencyEstim, qaKnowl) match
    // Сurr estimate takes precedence over qa estimates and sub estimates
    case (curr @ Some(_), _) => (curr, true)
    // Curr estimate is formed by qa estimates and sub estimates
    case (None, qa @ Some(_)) => (qa, false)
    // Curr estimate is formed by sub estimates
    case (None, None) => (None, false)

  if forceSet then
    // Mark childs as overriden by parent knowledge
    val subOverriden = sub.filterKeys(_.isChildOf(c.numeration)).mapValues(_.markOverriden(currKnowledge.get)).toMap
    return subOverriden + (c.numeration -> currKnowledge.get)

  // Collect child knowledges
  val sum = KnowledgeComputed.summarizer(c.numeration)
  val childKnowl = c.childs
    .map(_.numeration)
    .collect(n => sub.get(n) match { case Some(knowl) => knowl })
    .reduceOption(sum)

  val resultKnowl = (currKnowledge, childKnowl) match
    case (Some(parent), Some(child)) => Some(sum(parent, child))
    case (None, Some(child))         => Some(child.asSynthetic) // Competency formed only from child estimates
    case (parent @ Some(_), None)    => parent
    case (None, None)                => None
  return resultKnowl.map(kc => sub + (c.numeration -> kc)).getOrElse(sub)
end computeKnowledge

private def computeQAKnowledge(c: Competency): Option[KnowledgeComputed] =
  c.qa
    .map(qaKnowledge(c, _))
    .collect(_ match { case Some(knowl) => knowl })
    .reduceOption(KnowledgeComputed.summarizer(c.numeration))

private def qaKnowledge(c: Competency, qa: QA): Option[KnowledgeComputed] =
  val ktest = qa.knowledgeTest
  ktest.estimate match
    case KnowledgeEstimate.NotMentioned => None
    case KnowledgeEstimate.Answered(percent) =>
      val received = percent / 100f * ktest.maxPoints
      Some(KnowledgeComputed(numeration = c.numeration, maxPoints = ktest.maxPoints, receivedPoints = received))

def insertCompetency(top: Competencies, parent: Option[Numeration], name: String): Competencies =
  val tgtNumeration = parent match
    // None means no parent => insert at top
    case None => top.map(_.numeration).sorted(using NumerationOrdering).lastOption.map(_.next).getOrElse(Vector(1))
    case Some(parent) =>
      val tgtParent = top.flatMap(flatten(_)).find(_.numeration == parent) match
        case None    => return top
        case Some(p) => p
      tgtParent.childs.map(_.numeration).sorted(using NumerationOrdering).lastOption match
        case None    => tgtParent.numeration appended 1
        case Some(n) => n.next
  val tgt = userCreatedCompetency(tgtNumeration, name)
  if tgtNumeration.size == 1 then top appended tgt else top.map(c => insert(tgt, c))

/** Return copy of currParent with inserted tgt to childs */
private def insert(tgt: Competency, currParent: Competency): Competency =
  val tgtParent = tgt.numeration.directParent.get
  if currParent.numeration.isDirectParentOf(tgt.numeration) then
    currParent.copy(childs = currParent.childs appended tgt)
  else if !tgtParent.isChildOf(currParent.numeration) then currParent
  else currParent.copy(childs = currParent.childs.map(c => insert(tgt, c)))

private def userCreatedCompetency(n: Numeration, name: String) = Competency(
  numeration = n,
  name = name,
  qa = Seq.empty,
  childs = Seq.empty
)

private def flatten(competency: Competency): Competencies =
  val childs = if competency.childs.nonEmpty then competency.childs.flatMap(flatten(_)) else Seq.empty
  childs :+ competency

def qaInserter(question: String): Updater = (competency) =>
  val tgt = QA(question = question, answer = None)
  competency.copy(qa = competency.qa appended tgt)
