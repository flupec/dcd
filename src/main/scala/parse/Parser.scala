package parse

import common.ParseError

import java.util.regex.Pattern
import scala.annotation.tailrec

case class Readable(val value: String, val fullyRead: Boolean):
  def asRead = copy(fullyRead = true)
  def isEmpty = value.isEmpty()
  def concat(add: Char) = this.ensuring(!fullyRead).copy(value = value + add)
  def eraseRight(amount: Int) = copy(value = value.dropRight(amount))
  def result = value.trim

object Readable:
  def empty = Readable("", false)

case class ReadableNumber(val digits: Seq[Int], val fullyRead: Boolean):
  def asRead = copy(fullyRead = true)
  def isEmpty = digits.isEmpty
  def append(digit: Int) = this.ensuring(!fullyRead).copy(digits = digits appended digit)

object ReadableNumber:
  def empty = ReadableNumber(List.empty, false)

object Parser:

  /** Parses .dcd formatted input.
    *  Expects that file lines at [[ParseInput]] contains EOL ('\n' or '\r\n')
    *
    * @param in .dcd formatted
    * @return parse result
    */
  def parse(in: ParseInput): Either[ParseError, ParseResult] =
    val start = DcdParser.Empty
    doParse(in, ParseResult.empty, Cursor.atBegin, start)

  @tailrec
  private def doParse(
      in: ParseInput,
      out: ParseResult,
      cursor: Cursor,
      state: DcdParser
  ): Either[ParseError, ParseResult] =
    if out.over then Right(out)
    else
      state.next(in, cursor, out) match
        case Left(err)      => Left(err)
        case Right(r, c, s) => doParse(in, r, c, s)

// Parser states for .dcd file format
enum DcdParser derives CanEqual:
  case Empty
  case Setting(val key: Readable, val value: Readable)
  case CompetencyHeader(val numeration: ReadableNumber, val name: Readable)
  case CompetencyQA(val question: Readable, val answer: Readable)

  import DcdParser.*

  @tailrec
  final def next(
      input: ParseInput,
      cursor: Cursor,
      out: ParseResult
  ): Either[ParseError, (ParseResult, Cursor, DcdParser)] =
    if out.over then return Right(out, cursor, this)

    val nextResult = this match
      case Empty               => nextForEmpty(input, cursor, out)
      case _: Setting          => nextForSetting(input, cursor, out)
      case _: CompetencyHeader => nextForCompetencyHeader(input, cursor, out)
      case _: CompetencyQA     => nextForCompetencyQA(input, cursor, out)

    nextResult match
      case Left(err)       => Left(err)
      case Right(pr, c, p) => p.next(input, c, pr)
  end next

  private def nextForEmpty(
      input: ParseInput,
      currPos: Cursor,
      out: ParseResult
  ): Either[ParseError, (ParseResult, Cursor, DcdParser)] =
    val nextPos = currPos.next(input)
    if nextPos == Cursor.EOF then return Right(out.asOver, nextPos, Empty)

    input.at(nextPos) match
      // Setting declaration
      case SettingDeclaration => Right(out, nextPos, emptySetting)

      // Competency header
      case HeaderDeclaration => Right(out, nextPos, emptyCompetencyHeader)

      // Competency question and answer
      case QuestionDeclaration => Right(out, nextPos, emptyCompetencyQA)

      // Empty chars, do nothing and proceed further
      case nextChar if isSpaceOrTabOrEOL(nextChar) => Right(out, nextPos, this)

      // Incorrect grammar
      case wrongChar => Left(wrongFormat(wrongChar, nextPos, input))

  /** Responsible for settings via '! settingKey=settingValue' pattern */
  private def nextForSetting(
      input: ParseInput,
      currPos: Cursor,
      out: ParseResult
  ): Either[ParseError, (ParseResult, Cursor, DcdParser)] =
    val self = this.asInstanceOf[Setting]
    val nextPos = currPos.next(input)

    if nextPos == Cursor.EOF then
      return if self.key.fullyRead && self.value.fullyRead
      then Right(resultSetting(out, self).asOver, nextPos, Empty) // Handle EOF and store last read setting
      else Left(unexpectedParserState(currPos, input))

    val currChar = input.at(currPos)

    input.at(nextPos) match
      // Curr is setting declaration, next is space symbol => do nothing
      case nextChar if currChar == SettingDeclaration && isSpaceOrTab(nextChar) =>
        Right(out, nextPos, this)

      // Curr is space symbol, next is space symbol => do nothing
      case nextChar if isSpaceOrTab(currChar) && isSpaceOrTab(nextChar) =>
        Right(out, nextPos, this)

      // Curr is space symbol, next is alphabet symbol => add this next symbol to setting key or value
      case nextChar if isSpaceOrTab(currChar) && isText(nextChar) =>
        Right(out, nextPos, updatedSetting(nextChar, self))

      // Curr is alphabet symbol, next is alphabet symbol => append to setting key or value
      case nextChar if isText(currChar) && isText(nextChar) =>
        Right(out, nextPos, updatedSetting(nextChar, self))

      // Curr is alphabet symbol, next is '=' symbol => mark setting key as fully read
      case nextChar if isText(currChar) && nextChar == SettingEquality =>
        Right(out, nextPos, commitSettingKeyOrVal(self))

      // Curr is '=' symbol, next is alhabet symbol => append to setting value
      case nextChar if currChar == SettingEquality && isText(nextChar) =>
        Right(out, nextPos, updatedSetting(nextChar, self))

      // Curr is alphabet symbol, next is space symbol or EOL symbol => mark setting value as fully read
      case nextChar if isText(currChar) && isSpaceOrTabOrEOL(nextChar) =>
        Right(out, nextPos, commitSettingKeyOrVal(self))

      // Curr is space symbol, next is EOL => mark setting value as fully read
      case nextChar if isSpaceOrTab(currChar) && isEOL(nextChar) =>
        if self.value.isEmpty
        then Left(wrongFormat(nextChar, nextPos, input))
        else Right(out, nextPos, commitSettingKeyOrVal(self))

      // Curr is EOL, commit setting and move to empty state without cursor pushing
      case _ if isEOL(currChar) => Right(resultSetting(out, self), currPos, Empty)

      // Incorrect grammar
      case nextChar => Left(wrongFormat(nextChar, nextPos, input))
  end nextForSetting

  private def nextForCompetencyHeader(
      input: ParseInput,
      currPos: Cursor,
      out: ParseResult
  ): Either[ParseError, (ParseResult, Cursor, DcdParser)] =
    val self = this.asInstanceOf[CompetencyHeader]
    val nextPos = currPos.next(input)
    if nextPos == Cursor.EOF then
      return if self.name.fullyRead && self.numeration.fullyRead
      then resultCompetencyHeader(out, self).map(parseResult => (parseResult, currPos, Empty)) // Handle EOF
      else Left(unexpectedParserState(currPos, input))

    val currChar = input.at(currPos)
    input.at(nextPos) match
      // Curr char is header declaration, numeration is not fully read and nextChar is digit => append digit to numeration
      case nextChar if currChar == HeaderDeclaration && !self.numeration.fullyRead && nextChar.isDigit =>
        Right(out, nextPos, updatedHeaderNumeration(nextChar, self))

      // Curr char is header declaration, next char is space => do nothing
      case nextChar if currChar == HeaderDeclaration && isSpaceOrTab(nextChar) => Right(out, nextPos, self)

      // Curr char is space, numeration is not fully read and next char is digit => append digit to numeration
      case nextChar if isSpaceOrTab(currChar) && !self.numeration.fullyRead && nextChar.isDigit =>
        Right(out, nextPos, updatedHeaderNumeration(nextChar, self))

      // Curr char is digit, nextChar is digit => append digit
      case nextChar if currChar.isDigit && nextChar.isDigit =>
        Right(out, nextPos, updatedHeaderNumeration(nextChar, self))

      // Curr char is digit, nextChar is '.', but prev header not in result => wrong format error
      case nextChar
          if currChar.isDigit
            && nextChar == NumerationDelimeter
            && !isCurrentNumerationInResult(out, self, input) =>
        Left(wrongFormat(nextChar, nextPos, input))

      // Curr char is digit, nextChar is '.', prev header in result (handled above) => do nothing
      case nextChar if currChar.isDigit && nextChar == NumerationDelimeter => Right(out, nextPos, self)

      // Curr char is '.', nextChar is digit => add digit to numeration
      case nextChar if currChar == NumerationDelimeter && nextChar.isDigit =>
        Right(out, nextPos, updatedHeaderNumeration(nextChar, self))

      // Curr char is digit, nextChar is space => mark numeration as fully read
      case nextChar if currChar.isDigit && isSpaceOrTab(nextChar) =>
        Right(out, nextPos, commitHeaderNumeration(self))

      // Curr char is space, nextChar is space => do nothing
      case nextChar if isSpaceOrTab(currChar) && isSpaceOrTab(nextChar) => Right(out, nextPos, self)

      // TODO add support for '(', ')' etc symbols
      // Curr char is space, nextChar is alphabet symbol = append symbol to name
      case nextChar if isSpaceOrTab(currChar) && isText(nextChar) =>
        Right(out, nextPos, updatedHeaderName(nextChar, self))

      // Curr char is alphabet symbol, nextChar is alphabet symbol => append nextChar to name
      case nextChar if isText(currChar) && isText(nextChar) =>
        Right(out, nextPos, updatedHeaderName(nextChar, self))

      // Curr char is alphabet symbol, nextChar is space => append space to name
      case nextChar if isText(currChar) && isSpaceOrTab(nextChar) =>
        Right(out, nextPos, updatedHeaderName(nextChar, self))

      // nextChar is EOL and name is not empty => mark name as fully read
      case nextChar if isEOL(nextChar) && !self.name.isEmpty =>
        Right(out, nextPos, commitHeaderName(self))

      // nextChar is EOL and name empty => return error, wrong format
      case nextChar if isEOL(nextChar) && self.name.isEmpty => Left(wrongFormat(nextChar, nextPos, input))

      // Curr char is EOL => commit competency header and move to empty state without cursor pushing
      case _ if isEOL(currChar) =>
        resultCompetencyHeader(out, self).map(parseResult => (parseResult, currPos, Empty))

      // Incorrect grammar
      case nextChar => Left(wrongFormat(nextChar, nextPos, input))
  end nextForCompetencyHeader

  private def nextForCompetencyQA(
      input: ParseInput,
      currPos: Cursor,
      out: ParseResult
  ): Either[ParseError, (ParseResult, Cursor, DcdParser)] =
    val self = this.asInstanceOf[CompetencyQA]
    val nextPos = currPos.next(input)
    if nextPos == Cursor.EOF then
      return if self.question.fullyRead
      then resultQA(out, commitQA(self)).map(result => (result, currPos, Empty))
      else Left(unexpectedParserState(currPos, input))

    val currChar = input.at(currPos)
    input.at(nextPos) match
      // Curr char is '-', next char is space => do nothing
      case nextChar if currChar == QuestionDeclaration && isSpaceOrTab(nextChar) => Right(out, nextPos, self)

      // Curr char is '-', next char is alphabet symbol => append to qa
      case nextChar if currChar == QuestionDeclaration && isText(nextChar) =>
        Right(out, nextPos, updatedQA(nextChar, self))

      // Curr char is alphabet symbol, next char is alphabet symbol => append to qa
      case nextChar if isText(currChar) && isText(nextChar) =>
        Right(out, nextPos, updatedQA(nextChar, self))

      // Curr char is any, next char is EOL and next next char is EOF => mark qa as fully read
      case nextChar if isEOL(nextChar) && nextPos.next(input) == Cursor.EOF =>
        resultQA(out, commitQA(self)).map(result => (result, nextPos, Empty))

      // Curr char is alphabet symbol, next char is space => add space
      case nextChar if isText(currChar) && isSpaceOrTab(nextChar) =>
        Right(out, nextPos, updatedQA(nextChar, self))

      // Curr char is alphabet symbol, next char is EOL, question already read => add EOL to answer
      case nextChar if isText(currChar) && isEOL(nextChar) && self.question.fullyRead =>
        Right(out, nextPos, updatedQA(nextChar, self))

      // Curr char is alphabet symbol, next char is EOL => do nothing
      case nextChar if isText(currChar) && isEOL(nextChar) =>
        Right(out, nextPos, self)

      // Curr char is space, next char is space => ignore it
      case nextChar if isSpaceOrTabOrEOL(currChar) && isSpaceOrTabOrEOL(nextChar) => Right(out, nextPos, self)

      // Curr char is space, next char is alphabet symbol => symbol
      case nextChar if isSpaceOrTab(currChar) && isText(nextChar) =>
        Right(out, nextPos, updatedQA(nextChar, self))

      // Curr char is space, nextChar is '=' => erase previosly added space, mark question as fully read
      case nextChar if isSpaceOrTabOrEOL(currChar) && nextChar == AnswerDeclaration =>
        if !self.question.fullyRead then Right(out, nextPos, commitQA(dropRightQA(self)))
        else Left(wrongFormat(nextChar, nextPos, input))

      // Curr char is '=', next char is '=' => do nothing
      case nextChar if currChar == AnswerDeclaration && nextChar == AnswerDeclaration => Right(out, nextPos, self)

      // Curr char is '=', next char is space => do nothing
      case nextChar if currChar == AnswerDeclaration && isSpaceOrTabOrEOL(nextChar) => Right(out, nextPos, self)

      // Curr char is any, next char is '-' or '#' => mark qa as fully read without cursor pushing
      case nextChar if nextChar == QuestionDeclaration || nextChar == HeaderDeclaration =>
        resultQA(out, commitQA(self)).map(result => (result, currPos, Empty))

      // Incorrect grammar
      case nextChar => Left(wrongFormat(nextChar, nextPos, input))
end DcdParser

object DcdParser:
  val SettingDeclaration = '!'
  val SettingEquality = '='
  val HeaderDeclaration = '#'
  val NumerationDelimeter = '.'
  val QuestionDeclaration = '-'
  val AnswerDeclaration = '='

  // Punctuation and alphabetic symbols except .dcd specific identifiers
  private val TextPattern: Pattern = Pattern.compile("[\\p{Print}&&[^!=#-]]")

  def isSpaceOrTabOrEOL(c: Char) = isSpaceOrTab(c) || isEOL(c)
  def isSpaceOrTab(c: Char) = c == ' ' || c == '\t' || c == '\r'
  def isEOL(c: Char) = c == '\n'
  def isText(c: Char) = !isSpaceOrTabOrEOL(c) && TextPattern.matcher(String.valueOf(c)).matches()

  private def wrongFormat(char: Char, at: Cursor, in: ParseInput) =
    ParseError.BadFileFormat(s"Unexpected symbol at line=${at.line}, col=${at.column}, symbol='$char'", in.file)

  private def unexpectedParserState(at: Cursor, in: ParseInput) =
    ParseError.BadFileFormat(s"Something wrong near line=${at.line}, col=${at.column}", in.file)

  private def unexpectedParserState(err: String) = ParseError.InternalErr(err)

  private def emptySetting = Setting(Readable.empty, Readable.empty)
  private def emptyCompetencyHeader = CompetencyHeader(ReadableNumber.empty, Readable.empty)
  private def emptyCompetencyQA = CompetencyQA(Readable.empty, Readable.empty)

  private def updatedSetting(nextChar: Char, s: Setting): Setting =
    if !s.key.fullyRead then s.copy(key = s.key.concat(nextChar))
    else s.copy(value = s.value.concat(nextChar))

  private def commitSettingKeyOrVal(s: Setting): Setting =
    if !s.key.fullyRead then s.copy(key = s.key.asRead) else s.copy(value = s.value.asRead)

  private def resultSetting(r: ParseResult, s: Setting): ParseResult =
    // TODO Either.cond and validate
    r.copy(settings = r.settings appended (s.key.result, s.value.result))

  private def updatedHeaderNumeration(digit: Char, h: CompetencyHeader): CompetencyHeader =
    h.copy(numeration = h.numeration.append(digit.asDigit))

  private def isCurrentNumerationInResult(
      r: ParseResult,
      h: CompetencyHeader,
      in: ParseInput
  ): Boolean = r.findCompetency(h.numeration.digits).isDefined

  private def updatedHeaderName(c: Char, h: CompetencyHeader): CompetencyHeader =
    h.copy(name = h.name.concat(c))

  private def commitHeaderNumeration(h: CompetencyHeader) = h.copy(numeration = h.numeration.asRead)
  private def commitHeaderName(h: CompetencyHeader) = h.copy(name = h.name.asRead)

  private def resultCompetencyHeader(
      parseResult: ParseResult,
      header: CompetencyHeader
  ): Either[ParseError, ParseResult] =
    val numeration = header.numeration.digits
    val name = header.name.result
    Either
      .cond(numeration.nonEmpty && name.nonEmpty, numeration, unexpectedParserState("Numeration or name is empty"))
      .map(numeration => ParsedCompetency(numeration, name, List.empty, List.empty))
      .flatMap(parseResult.withCompetency)
      .map(c => c.copy(lastReadCompetency = Some(numeration)))

  private def updatedQA(symb: Char, q: CompetencyQA): CompetencyQA =
    if !q.question.fullyRead then q.copy(question = q.question.concat(symb))
    else q.copy(answer = q.answer.concat(symb))

  private def dropRightQA(q: CompetencyQA): CompetencyQA = q.copy(question = q.question.eraseRight(1))

  private def commitQA(q: CompetencyQA): CompetencyQA =
    if !q.question.fullyRead then q.copy(question = q.question.asRead)
    else q.copy(answer = q.answer.asRead)

  private def resultQA(parseResult: ParseResult, q: CompetencyQA): Either[ParseError, ParseResult] =
    val question = q.question.result
    val answer = Option.when(q.answer.fullyRead)(q.answer.result)
    latestReadCompetency(parseResult)
      .fold(Left(unexpectedParserState("Attempt to create QA without competency context")))(Right(_))
      .filterOrElse(_ => question.nonEmpty, unexpectedParserState("Question body must exist"))
      .flatMap(competency => parseResult.withCompetency(competency.withQA(ParsedQA(question, answer))))

  private def latestReadCompetency(out: ParseResult): Option[ParsedCompetency] =
    out.lastReadCompetency.flatMap(out.findCompetency)
