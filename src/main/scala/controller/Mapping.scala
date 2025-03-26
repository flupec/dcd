package controller

private def toKnowledgeTest(kc: view.KnowledgeCompleteness): model.KnowledgeTest =
  model.KnowledgeTest(estimate = toModelEstimate(kc))

def toModelEstimate(kc: view.KnowledgeCompleteness): model.KnowledgeEstimate = kc match
  case view.KnowledgeCompleteness.NotMentioned      => model.KnowledgeEstimate.NotMentioned
  case view.KnowledgeCompleteness.Answered(percent) => model.KnowledgeEstimate.Answered(percent)
  case view.KnowledgeCompleteness.Unanswered        => model.KnowledgeEstimate.Answered(0)

/** Returns flattened representation */
def toView(cs: model.Competencies): Seq[view.CompetencyView] = cs.flatMap(toCompetencyView(_))

private def toCompetencyView(src: model.Competency): Seq[view.CompetencyView] =
  val childs = if src.childs.nonEmpty then src.childs.flatMap(toCompetencyView(_)) else List.empty
  childs :+
    view.CompetencyView(
      name = src.name,
      numeration = src.numeration,
      completeness = toViewCompleteness(src.estimate),
      questions = toViewQA(src.qa),
      notes = src.notes
    )

private def toViewQA(src: Seq[model.QA]): Seq[view.QA] = src.map: q =>
  view.QA(questionBody = q.question, answerBody = q.answer, status = toViewCompleteness(q.knowledgeTest.estimate))

private def toViewCompleteness(e: model.KnowledgeEstimate): view.KnowledgeCompleteness = e match
  case model.KnowledgeEstimate.NotMentioned                      => view.KnowledgeCompleteness.NotMentioned
  case model.KnowledgeEstimate.Answered(percent) if percent == 0 => view.KnowledgeCompleteness.Unanswered
  case model.KnowledgeEstimate.Answered(percent)                 => view.KnowledgeCompleteness.Answered(percent)

def toKnowledgeComputedView(kc: model.KnowledgeComputed): view.KnowledgeComputed =
  if kc.maxPoints == 0f then view.KnowledgeComputed(0, kc.overridenBy.map(_.numeration), kc.synthetic)
  else
    view.KnowledgeComputed(
      (kc.receivedPoints / kc.maxPoints * 100f).toInt,
      kc.overridenBy.map(_.numeration),
      kc.synthetic
    )
