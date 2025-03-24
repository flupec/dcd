package view

import common.InputError

type InputValidator = (String) => Either[common.Error, String]

def onlyDigitsValidator: InputValidator = s =>
  Either.cond(s.forall(_.isDigit), s, InputError.RestrictedSymbol("0123456789"))

def numRangeValidator(min: Int, max: Int): InputValidator = s =>
  s.toIntOption match
    case Some(value) if min <= value && value <= max => Right(s)
    case Some(value) => Left(InputError.IncorrectValue(s"'$value' must be in range=[$min, $max]"))
    case None        => Left(InputError.IncorrectValue("non parseable integer"))
