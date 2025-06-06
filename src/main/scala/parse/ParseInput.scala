package parse

import java.io.File
import java.nio.file.Path
import common.ParseError
import scala.util.Try
import scala.jdk.CollectionConverters._
import java.nio.file.Files

case class ParseInput(
    val file: File,
    val fileLines: Seq[String] // per line
):
  def at(c: Cursor): Char =
    require(c != Cursor.EOF)
    fileLines(c.line)(c.column)

  /** Returns slice of input data
    *
    * @param from exclusive slice start
    * @param n slice size
    * @return sequence of input chars. Will contain None If input is not large enough
    */
  def slice(from: Cursor, n: Int): Seq[Option[Char]] =
    return from
      .nextMulti(this, n)
      .map:
        _ match
          case Cursor.EOF       => None
          case c @ Cursor(_, _) => Some(at(c))
  end slice

object ParseInput:
  def fromFile(at: Path): Either[ParseError, ParseInput] =
    Try(Files.readAllLines(at).asScala)
      .map(Vector.from)
      .map(lines => lines.map(line => line.concat(System.lineSeparator())))
      .map(lines => ParseInput(at.toFile(), lines))
      .toEither
      .left
      .map(e => ParseError.FileOperationErr(e.getMessage()))

case class Cursor(
    val line: Int,
    val column: Int
) derives CanEqual:
  def next(in: ParseInput): Cursor =
    if this == Cursor.EOF then return Cursor.EOF
    if !lineExists(line, in) then return Cursor.EOF
    val (nextLine, nextColumn) =
      if positionExists(line, column + 1, in) then (line, column + 1) else (line + 1, 0)

    if positionExists(nextLine, nextColumn, in) then Cursor(nextLine, nextColumn) else Cursor.EOF
  end next

  private def lineExists(line: Int, in: ParseInput): Boolean =
    return line < in.fileLines.size

  private def positionExists(line: Int, column: Int, in: ParseInput): Boolean =
    lineExists(line, in) && column < in.fileLines(line).size

  /** Proceeds current cursor forward up to multiple positions
    *
    * @param in cursor input
    * @param n cursor propagation amount
    * @return multiple cursors
    */
  def nextMulti(in: ParseInput, n: Int): Seq[Cursor] =
    require(n >= 0)
    if n == 0 then return Vector.empty
    if n == 1 then return Vector(next(in))
    val previousCursors = nextMulti(in, n - 1)
    return previousCursors appended previousCursors.last.next(in)
  end nextMulti
end Cursor

object Cursor:
  val EOF = Cursor(-1, -1)

  def atBegin = Cursor(0, -1)
