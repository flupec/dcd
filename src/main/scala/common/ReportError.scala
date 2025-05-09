package common

enum ReportError extends Error derives CanEqual:
  case GenerateError

  override def reason: String = this match
    case GenerateError => "Cannot generate report file"
    
  