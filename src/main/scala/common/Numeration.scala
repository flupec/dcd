package common

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

/** Lexiographic order */
object NumerationOrdering extends Ordering[Numeration]:
  override def compare(x: Numeration, y: Numeration): Int =
    // Stupid but easy solution
    val stringify = (n: Numeration) =>
      n.map(_.toString).reduceLeft((l, r) => l.concat(r))

    stringify(x).compare(stringify(y))
