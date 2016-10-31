package org.viacheslav.rodionov.externalsorting

import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Buffer

object readers {

  case class FileStringReader(filePath: String) extends ExternalAccess {
    private lazy val reader: Iterator[String] = fileLines(filePath)
    private var headOption: Option[String] = None

    def currentHeadOption: Option[String] = headOption match {
      case Some(str) => Some(str)
      case None => {
        this.next()
        this.headOption
      }
    }

    def next(): Option[String] = if (reader.hasNext) {
      val str = reader.next
      this.headOption = Some(str)
      Some(str)
    } else {
      if (this.headOption.isDefined) this.headOption = None
      None
    }

  }

  object FileStringReader {
    val ord: Ordering[FileStringReader] = Ordering.by { reader: FileStringReader =>
      reader.currentHeadOption
    }.reverse

    def apply(file: File): FileStringReader = FileStringReader(file.getAbsoluteFile)
  }

  case class FilesReader(filePaths: String*) {
    private lazy val readers: mutable.Buffer[FileStringReader] = filePaths.map(FileStringReader(_)).toBuffer
    var lastReader: Option[Int] = if (readers.nonEmpty) Some(0) else None

    @tailrec
    final def readLine(): Option[String] = lastReader match {
      case Some(i) => readers(i) next() match {
        case Some(str) => Some(str)
        case None => {
          if (readers.size > 1) {
            readers remove i
            lastReader = Some(i % readers.size)
            readLine()
          } else {
            None
          }
        }
      }
      case None => None
    }
  }


}