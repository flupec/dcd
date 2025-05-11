package controller

import common.*
import model.Competencies
import model.KnowledgeComputed
import model.byNumeration
import model.computeKnowledge
import model.estimateUpdater
import model.insertCompetency
import model.qaInserter
import model.qaKnowledgeUpdater
import model.updateCompetencies
import result.ResultExporter
import view.CompetencyView
import view.KnowledgeCompleteness

trait CompetenciesController:
  /** Returns current state of competencies */
  def competencies: Seq[CompetencyView]

  /** Estimate given competency and return current state */
  def estimatedCompetency(competency: Numeration, est: KnowledgeCompleteness): Seq[CompetencyView]

  /** Estimate QA in given competency and return current state */
  def estimatedQA(
      competency: Numeration,
      qaIndex: Int,
      est: KnowledgeCompleteness
  ): Seq[CompetencyView]

  def computedKnowledges: Map[Numeration, view.KnowledgeComputed]

  /** Create new competency at given parent. If competency must be at 0 nest level, then parent must be None */
  def createCompetency(parent: Option[Numeration], name: String): Seq[CompetencyView]

  /** Create new QA in given competency with specified QA question */
  def createQA(competency: Numeration, question: String): Seq[CompetencyView]

  def exportKnowledges: Unit
end CompetenciesController

class CompetenciesControllerImpl(
    private val resultMgmt: ResultExporter,
    private var state: Competencies
) extends CompetenciesController:

  override def competencies: Seq[CompetencyView] = toView(state)

  override def estimatedCompetency(
      competency: Numeration,
      est: KnowledgeCompleteness
  ): Seq[CompetencyView] =
    state = updateCompetencies(state, byNumeration(competency), estimateUpdater(toModelEstimate(est)))
    toView(state)

  override def estimatedQA(
      competency: Numeration,
      qaIndex: Int,
      est: KnowledgeCompleteness
  ): Seq[CompetencyView] =
    state = updateCompetencies(state, byNumeration(competency), qaKnowledgeUpdater(toKnowledgeTest(est), qaIndex))
    toView(state)

  override def computedKnowledges: Map[Numeration, view.KnowledgeComputed] =
    val modelKnowledges = computedCompetencyKnowledges
    return modelKnowledges.view.mapValues(toKnowledgeComputedView(_)).toMap

  private def computedCompetencyKnowledges: Map[Numeration, KnowledgeComputed] =
    val roots = state.filter(_.numeration.size == 1)
    val knowledges: Seq[Map[Numeration, KnowledgeComputed]] = roots.map(computeKnowledge(_))
    val merged = knowledges.reduce((l, r) => l concat r)
    return merged

  override def createCompetency(parent: Option[Numeration], name: String): Seq[CompetencyView] =
    state = insertCompetency(state, parent, name)
    toView(state)

  override def createQA(competency: Numeration, question: String): Seq[CompetencyView] =
    state = updateCompetencies(state, byNumeration(competency), qaInserter(question))
    toView(state)

  override def exportKnowledges: Unit =
    val competencyKnowl = computedCompetencyKnowledges.values.toList
    val qaKnowl = state.flatMap(_.flatten).flatMap(toQAKnowledgeResult)
    resultMgmt.doExport(competencyKnowl, qaKnowl)
end CompetenciesControllerImpl
