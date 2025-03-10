package model

import java.io.File
import common.ParseError
import parse.ParseInput
import parse.Parser
import parse.ParsedCompetency

type CompetenciesFactory = (File) => Either[ParseError, Seq[Competency]]

def readCompetencies(file: File): Either[ParseError, Seq[Competency]] =
  for
    input <- ParseInput.fromFile(file.toPath())
    parseResult <- Parser.parse(input)
    competencies = fromParsedCompetencies(parseResult.competencies)
  yield competencies

private def fromParsedCompetencies(p: Seq[ParsedCompetency]): Seq[Competency] = p.map(fromParsedCompetency(_))

private def fromParsedCompetency(p: ParsedCompetency): Competency = ???
// Competency(
//   numeration = p.numeration,
//   qa = null, // TODO
//   childs = null,
//   estimate = KnowledgeEstimate.NotMentioned,
//   computed = None
// )
