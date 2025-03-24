package common

enum InputError extends Error:
  case RestrictedSymbol(allowedSymbols: String)
  case IncorrectValue(info: String)

  override def reason: String = this match
    case RestrictedSymbol(allowedSymbols) => s"Incorrect symbol, allowed=$allowedSymbols"
    case IncorrectValue(info)             => s"Got incorrect value: $info"
