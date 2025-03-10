package common

enum CliError extends Error:
  case MissedMandatoryArg(argKey: String)
  
  override def reason: String = this match
    case MissedMandatoryArg(argKey) => s"$argKey is mandatory option"
    
  
