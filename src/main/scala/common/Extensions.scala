package common

object EitherExtension:
  extension [A, B](e: Seq[Either[A, B]])
    def sequenceRight: Either[A, Seq[B]] =
      val (errs, goods) = e.partitionMap(identity)
      if errs.nonEmpty then Left(errs.head) else Right(goods)

