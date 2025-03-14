package model

import java.io.File
import common.ParseError
import parse.ParseInput
import parse.Parser
import parse.ParsedCompetency
import parse.ParsedQA

def readCompetencies(file: File): Either[ParseError, Seq[Competency]] =
  for
    input <- ParseInput.fromFile(file.toPath())
    parseResult <- Parser.parse(input)
    competencies = fromParsedCompetencies(parseResult.competencies)
  yield competencies

private def fromParsedCompetencies(p: Seq[ParsedCompetency]): Seq[Competency] = p.map(fromParsedCompetency(_))

private def fromParsedCompetency(p: ParsedCompetency): Competency =
  val childs = if p.childs.nonEmpty then p.childs.map(fromParsedCompetency(_)) else List.empty
  Competency(
    numeration = p.numeration,
    name = p.name,
    qa = p.qa.map(fromParsedQA(_)),
    childs = childs
  )

private def fromParsedQA(qa: ParsedQA): QA = QA(
  question = qa.question,
  answer = qa.answer
)
