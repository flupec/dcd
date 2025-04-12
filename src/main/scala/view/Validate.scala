package view

import common.InputError.IncorrectValue
import common.InputError.RestrictedSymbol

type InputValidator = (String) => Either[common.Error, String]

def onlyDigitsValidator: InputValidator = s => Either.cond(s.forall(_.isDigit), s, RestrictedSymbol("0123456789"))

def numRangeValidator(min: Int, max: Int): InputValidator = s =>
  s.toIntOption match
    case Some(value) if min <= value && value <= max => Right(s)
    case Some(value)                                 => Left(IncorrectValue(s"'$value' must be in range=[$min, $max]"))
    case None                                        => Left(IncorrectValue("non parseable integer"))

def notEmptyValidator: InputValidator = s => Either.cond(s.nonEmpty, s, IncorrectValue("Must be non empty"))
