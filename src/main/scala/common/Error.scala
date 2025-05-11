package common

import java.io.File

trait Error:
  def reason: String

enum CliError extends Error:
  case MissedMandatoryArg(argKey: String)
  case BadArgumentFormat(argKey: String)

  override def reason: String = this match
    case MissedMandatoryArg(argKey) => s"'$argKey' is mandatory option"
    case BadArgumentFormat(argKey)  => s"unexpected format for '$argKey'"
end CliError

enum InputError extends Error:
  case RestrictedSymbol(allowedSymbols: String)
  case IncorrectValue(info: String)

  override def reason: String = this match
    case RestrictedSymbol(allowedSymbols) => s"Incorrect symbol, allowed=$allowedSymbols"
    case IncorrectValue(info)             => s"Got incorrect value: $info"
end InputError

enum ParseError extends Error:
  case BadFileFormat(val report: String, val file: File)
  case FileOperationErr(val err: String)
  case InternalErr(val err: String)

  override def reason: String = this match
    case BadFileFormat(report, file) => s"Unexpected formatting at ${file.getPath()}: $report"
    case FileOperationErr(err)       => err
    case InternalErr(err)            => err
end ParseError

enum ReportError extends Error derives CanEqual:
  case Unexpected(report: String)
  case InconsistentSourceDescriptor(report: String)

  override def reason: String = this match
    case Unexpected(report)                   => s"Cannot generate report file: $report"
    case InconsistentSourceDescriptor(report) => s"Inconsistent source descriptor: $report"
end ReportError

object ReportError:
  def notFoundNumeration(n: Numeration) = InconsistentSourceDescriptor(s"Cannot find competency with $n numeration")
end ReportError

enum ResultExportError extends Error:
  case FileOpErr(val report: String)

  override def reason: String = this match
    case FileOpErr(report) => s"Cannot export results: $report"
end ResultExportError

enum ResultImportError extends Error:
  case FileOpErr(val report: String)

  override def reason: String = this match
    case FileOpErr(report) => s"Cannot import results: $report"
end ResultImportError
