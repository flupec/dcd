package common

enum CliError extends Error:
  case MissedMandatoryArg(argKey: String)
  case BadArgumentFormat(argKey: String)

  override def reason: String = this match
    case MissedMandatoryArg(argKey) => s"'$argKey' is mandatory option"
    case BadArgumentFormat(argKey)  => s"unexpected format for '$argKey'"
