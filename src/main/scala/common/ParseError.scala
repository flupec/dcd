package common

import java.io.File

enum ParseError extends Error:
  case BadFileFormat(val report: String, val file: File)
  case FileOperationErr(val err: String)
  case InternalErr(val err: String)

  override def reason: String = this match
    case BadFileFormat(report, file) => s"Unexpected formatting at ${file.getPath()}: $report"
    case FileOperationErr(err)       => err
    case InternalErr(err)            => err
