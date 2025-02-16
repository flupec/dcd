package view

class CompetencyView(
    val name: String,
    val numeration: Numeration,
    val completeness: KnowledgeCompleteness = KnowledgeCompleteness.NotMentioned,
    val decomposed: Seq[CompetencyView],
    val questions: Seq[QA],
    val notes: Seq[String] = Seq.empty
):
  override def toString(): String = s"{numeration=$numeration, name=$name}"

  override def equals(that: Any): Boolean =
    if that.isInstanceOf[CompetencyView] then numeration == that.asInstanceOf[CompetencyView].numeration
    else false

  override def hashCode(): Int = numeration.hashCode()

  def numerationView: String = numeration
    .map(_.toString)
    .reduceLeft((left, right) => s"${left.toString}.${right.toString}")

type Numeration = Seq[Int]

extension (curr: Numeration)

  /** Returns numeration with same amount of items, but with last item incremented
    *
    * @return next numeration
    */
  def next: Numeration =
    if curr.isEmpty then List.empty
    else curr.updated(curr.size - 1, curr.last + 1)

  /** Returns numeration with same amount of items, but with last item decremented
    *
    * @return previous numeration
    */
  def previous: Numeration =
    if curr.isEmpty then List.empty
    else curr.updated(curr.size - 1, 1.max(curr.last - 1))

  /** Checks if curr numeration is child to argument numeration
    *
    * @param parent possible parent
    * @return flag
    */
  def isChildOf(parent: Numeration): Boolean =
    if curr.size < 2 || curr.size < parent.size + 1 then false
    else if curr.size == parent.size + 1 then parent == curr.slice(0, curr.size - 1)
    else curr.directParent.map(currParent => currParent.isChildOf(parent)).getOrElse(false)

  /** Returns direct parent of this */
  def directParent: Option[Numeration] = Option.when(curr.size > 1)(curr.slice(0, curr.size - 1))
end extension

case class QA(
    val questionBody: String,
    val status: KnowledgeCompleteness = KnowledgeCompleteness.NotMentioned,
    answerBody: Option[String] = Option.empty
)

enum KnowledgeCompleteness:
  case NotMentioned
  case Answered(val completenessPercent: Int)
  case Unanswered
