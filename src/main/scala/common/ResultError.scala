package common

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
