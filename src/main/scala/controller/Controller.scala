package controller

import common.*
import model.Competencies
import model.KnowledgeComputed
import model.byNumeration
import model.computeKnowledge
import model.estimateUpdater
import model.qaKnowledgeUpdater
import model.readCompetencies
import model.updateCompetencies
import view.CompetencyView
import view.KnowledgeCompleteness

import java.io.File

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

class CompetenciesControllerImpl(private var state: Competencies) extends CompetenciesController:
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
    val roots = state.filter(_.numeration.size == 1)
    val knowledges: Seq[Map[Numeration, KnowledgeComputed]] = roots.map(computeKnowledge(_))
    val merged = knowledges.reduce((l, r) => l concat r)
    merged.view.mapValues(toKnowledgeComputedView(_)).toMap

object CompetenciesControllerImpl:
  def create(competenciesSource: File): Either[common.Error, CompetenciesController] =
    readCompetencies(competenciesSource).map(CompetenciesControllerImpl(_))
