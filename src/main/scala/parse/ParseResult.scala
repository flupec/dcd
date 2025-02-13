package parse

import common.ParseError

case class ParseResult(
    val competencies: Seq[ParsedCompetency],
    val settings: Seq[(String, String)], // (key, value) pair
    val over: Boolean,
    val lastReadCompetency: Option[Seq[Int]] = None // TODO it's a hack, must be a context in Parser
):
  override def toString(): String = s"competencies=$competencies, settings=$settings, over=$over"
  def asOver: ParseResult = this.copy(over = true)

  private def merged(what: ParsedCompetency): ParseResult =
    competencies.indexWhere(c => c.numeration == what.numeration) match
      case -1  => copy(competencies = competencies :+ what)
      case idx => copy(competencies = competencies.updated(idx, competencies(idx).mergedShallow(what)))

  def withCompetency(add: ParsedCompetency): Either[ParseError, ParseResult] =
    if add.numeration.isEmpty then Left(ParseError.InternalErr("TODO"))
    else if add.numeration.size == 1 then Right(merged(add))
    else
      // Insert must happen to competency with this numeration
      val parentNumeration = add.numeration.dropRight(1)
      findCompetencyRoot(parentNumeration) match
        case None => Left(ParseError.InternalErr(s"Cannot find competency with $parentNumeration numeration"))
        case Some(insertRoot) =>
          insertRoot
            .withInserted(add)
            .map(top => (competencies.indexWhere(_.numeration == top.numeration), top))
            .filterOrElse((idx, _) => idx >= 0, ParseError.InternalErr("TODO"))
            .map((idx, top) => copy(competencies = competencies.updated(idx, top)))

  private def findCompetencyRoot(numeration: Seq[Int]): Option[ParsedCompetency] =
    findCompetencyInternal(numeration).map((root, _) => root)

  def findCompetency(numeration: Seq[Int]): Option[ParsedCompetency] =
    findCompetencyInternal(numeration).map((_, found) => found)

  /** Returns (root, competency) pair */
  private def findCompetencyInternal(numeration: Seq[Int]): Option[(ParsedCompetency, ParsedCompetency)] =
    val found = competencies
      .map(root => (root, ParsedCompetency.findByNumeration(root, numeration)))
      .filter((_, subchild) => subchild.isDefined)

    found.headOption match
      case Some(root: ParsedCompetency, Some(competency)) => Some(root, competency)
      case _                                              => None

object ParseResult:
  def empty = ParseResult(Vector.empty, Vector.empty, false)

// TODO (refactor): It must be not a case class since identity is based on numeration
/** Tree structure */
case class ParsedCompetency(
    val numeration: Seq[Int],
    val name: String,
    val qa: Seq[ParsedQA],
    val childs: Seq[ParsedCompetency]
):
  override def toString(): String =
    s"{numeration=$numeration, name=$name, qa=$qa, childs=$childs}"

  def withQA(q: ParsedQA) = copy(qa = qa :+ q)

  /** Returns new tree copy */
  def withInserted(insertable: ParsedCompetency): Either[ParseError, ParsedCompetency] =
    if insertable.numeration.isEmpty then
      return Left(ParseError.InternalErr(s"Cannot insert competency with zero numeration to $numeration"))

    if numeration.size >= insertable.numeration.size then
      Left(ParseError.InternalErr(s"Cannot insert ${insertable.numeration} competency to $numeration competency"))
    else if numeration.size == insertable.numeration.size - 1 then Right(mergeToChilds(insertable))
    else
      // Go to child
      val childNumeration = insertable.numeration.slice(0, numeration.size + 1)
      childs.indexWhere(c => c.numeration == childNumeration) match
        case -1 => Left(ParseError.InternalErr(s"Not found numeration=$childNumeration at $numeration"))
        case childIdx =>
          childs(childIdx)
            .withInserted(insertable)
            .map(withInserted => copy(childs = childs.updated(childIdx, withInserted)))

  def mergedShallow(from: ParsedCompetency): ParsedCompetency =
    val missed = from.qa.filter(q => !qa.contains(q))
    copy(qa = qa :++ missed)

  private def mergeToChilds(child: ParsedCompetency): ParsedCompetency =
    childs.indexWhere(c => c.numeration == child.numeration) match
      case -1  => copy(childs = childs :+ child)
      case idx => copy(childs = childs.updated(idx, childs(idx).mergedShallow(child)))

object ParsedCompetency:
  def findByNumeration(competency: ParsedCompetency, searchNumeration: Seq[Int]): Option[ParsedCompetency] =
    if searchNumeration.isEmpty then return None
    if competency.numeration == searchNumeration then Some(competency)
    else
      val foundChilds = (for c <- competency.childs yield findByNumeration(c, searchNumeration)).filter(_.isDefined)
      foundChilds match
        case competency :: _ => competency
        case Nil             => None

case class ParsedQA(val question: String, val answer: Option[String] = None):
  override def toString(): String = s"{question=$question, answer=$answer}"
