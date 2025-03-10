package model

import common.Numeration
import munit.FunSuite

class CompetencyOpsTest extends FunSuite:
  test("Update competencies at top level list by numeration"):
    val src = List(competency(List(1, 1)), competency(List(1, 2)))
    val changedEstimate = KnowledgeEstimate.Answered(10)
    val estimateChanger = (c: Competency) => c.copy(estimate = changedEstimate)

    for updateTarget <- src.map(_.numeration) do
      val changed = updateCompetencies(src, byNumeration(updateTarget), estimateChanger)
      changed.find(_.numeration == updateTarget) match
        case None          => assert(false, s"Not updated competency with $updateTarget numeration")
        case Some(updated) => assert(updated.estimate == changedEstimate)

  test("Update child competencies by numeration"):
    lazy val parent = competency(List(1)).copy(childs = List(child))
    lazy val child = competency(List(1, 1))
    val changedEstimate = KnowledgeEstimate.Answered(10)
    val estimateChanger = (c: Competency) => c.copy(estimate = changedEstimate)

    val changedChilds = for
      changed <- updateCompetencies(List(parent), byNumeration(child.numeration), estimateChanger)
      changedChild <- changed.childs
      if changedChild.numeration == child.numeration
    yield changedChild

    assert(changedChilds.size == 1)
    assert(changedChilds.head.estimate == changedEstimate)

  test("Update QA knowledge with various indices"):
    val knowledge = KnowledgeTest(estimate = KnowledgeEstimate.NotMentioned)
    val qa = List(QA("Q1", None, knowledge), QA("Q2", None, knowledge))
    val src = List(competency(List(1)).copy(qa = qa))
    val changedKnowledge = KnowledgeTest(estimate = KnowledgeEstimate.Answered(10))

    for i <- 0 to 1 do
      val updated = updateCompetencies(src, byNumeration(src(0).numeration), qaKnowledgeUpdater(changedKnowledge, i))
      assert(updated.head.qa.size == qa.size)
      assert(updated.head.qa(i).knowledgeTest == changedKnowledge)

    for i <- 2 to 10 do
      val updated = updateCompetencies(src, byNumeration(src(0).numeration), qaKnowledgeUpdater(changedKnowledge, i))
      assert(updated.head.qa.size == qa.size)
      assert(updated.head.qa.forall(q => q.knowledgeTest == knowledge)) // not changed

  test("Compute knowledge should return no knowledge when all estimates si not mentioned"):
    val c = competency(List(1))
    assert(computeKnowledge(c).isEmpty)

  test("Compute knowledge should return knowledge from single competency estimate"):
    val estimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(50)
    val c = competency(List(1), KnowledgeEstimate.Answered(50))
    val knowledges = computeKnowledge(c)
    assert(knowledges.nonEmpty)
    assert(knowledges.contains(c.numeration))

    val k = knowledges(c.numeration)
    assertEquals(k.maxPoints, 1f)
    assertEquals(k.receivedPoints, estimate.percent / 100f)

  test("Compute knowledge should return knowledge from qa estimates when competency knowledge not estimated"):
    val qaEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(25)
    lazy val qa = QA("Q", None, KnowledgeTest(5f, qaEstimate))

    val c = competency(List(1)).copy(qa = List(qa))
    val knowledges = computeKnowledge(c)

    assert(knowledges.nonEmpty)
    assert(knowledges.contains(c.numeration))

    val k = knowledges(c.numeration)
    assertEquals(k.maxPoints, qa.knowledgeTest.maxPoints)
    assertEquals(k.receivedPoints, qa.knowledgeTest.maxPoints * qaEstimate.percent / 100f)

  test("Compute knowledge should return knowledge from competency estimate when qa estimates presents too"):
    val qaEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(25)
    lazy val qa = QA("Q", None, KnowledgeTest(5f, qaEstimate))

    val competencyEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(50)
    val c = competency(List(1), competencyEstimate).copy(qa = List(qa))
    val knowledges = computeKnowledge(c)

    assert(knowledges.nonEmpty)
    assert(knowledges.contains(c.numeration))

    val k = knowledges(c.numeration)
    assertEquals(k.maxPoints, 1f)
    assertEquals(k.receivedPoints, competencyEstimate.percent / 100f)

  test("Compute knowledge should return child knowledges"):
    lazy val childEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(25)
    lazy val child = competency(List(1, 1), childEstimate)
    val parentEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(50)
    val parent = competency(List(1), parentEstimate).copy(childs = List(child))
    val knowledges = computeKnowledge(parent)

    assert(knowledges.nonEmpty)
    assert(knowledges.contains(parent.numeration))
    assert(knowledges.contains(child.numeration))

    val gotChild = knowledges(child.numeration)
    assertEquals(gotChild.maxPoints, 1f)
    assertEquals(gotChild.receivedPoints, childEstimate.percent / 100f)

    val gotParent = knowledges(parent.numeration)
    assertEquals(gotParent.maxPoints, 1f) // Force set
    assertEquals(gotParent.receivedPoints, parentEstimate.percent / 100f) // Force set

  test("Compute knowledge should summarized child knowledges when parent knowledge estimate not present"):
    lazy val child1Estimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(25)
    lazy val child1 = competency(List(1, 1), child1Estimate)
    lazy val child2Estimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(50)
    lazy val child2 = competency(List(1, 2), child2Estimate)

    val parent = competency(List(1)).copy(childs = List(child1, child2))
    val knowledges = computeKnowledge(parent)

    assert(knowledges.nonEmpty)
    assert(knowledges.contains(parent.numeration))
    assert(knowledges.contains(child1.numeration))

    val k = knowledges(parent.numeration)
    assertEquals(k.maxPoints, 1f * parent.childs.size)

    val expectedPoints = parent.childs
      .map(c => c.estimate)
      .collect(_ match { case KnowledgeEstimate.Answered(percent) => percent })
      .sum / 100f
    assertEquals(k.receivedPoints, expectedPoints) // Force set

  test("Compute knowledge should return knowledge from qa and childs when parent competency knowledge not estimated"):
    val qaEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(25)
    lazy val qa = QA("Q", None, KnowledgeTest(5f, qaEstimate))

    val childEstimate: KnowledgeEstimate.Answered = KnowledgeEstimate.Answered(50)
    lazy val child = competency(List(1, 1), childEstimate)

    val c = competency(List(1)).copy(qa = List(qa), childs = List(child))
    val knowledges = computeKnowledge(c)

    assert(knowledges.nonEmpty)
    assert(knowledges.contains(c.numeration))
    assert(knowledges.contains(child.numeration))

    val k = knowledges(c.numeration)
    assertEquals(k.maxPoints, qa.knowledgeTest.maxPoints + 1f) // Parent QA max points + child estimate
    assertEquals(
      k.receivedPoints,
      qa.knowledgeTest.maxPoints * qaEstimate.percent / 100f + childEstimate.percent / 100f
    )

  private def competency(n: Numeration): Competency = competency(n, KnowledgeEstimate.NotMentioned)

  private def competency(n: Numeration, e: KnowledgeEstimate) = Competency(n, n.toString, List.empty, List.empty, e)
