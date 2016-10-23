package org.viacheslav.rodionov

import java.io.PrintWriter

import scala.collection.mutable.{Buffer, PriorityQueue}
import scala.io.Source._


trait External {
  def fileLines(path: String) = fromFile(path).getLines
}

case class ExternalSorting(filePath: String, capacity: Int) extends External {
  //TODO: doc: an iterator over the lines of a file (memory-friendly)
  //TODO: use buffer
  lazy val lines: Iterator[String] = fileLines(filePath)

  def readIntoMemory: Iterator[String] = lines take capacity

  def writeIntoFile(sortedLines: Buffer[String]): String = {
    val tempFile = java.io.File.createTempFile("extsrttbl", ".txt")
    new PrintWriter(tempFile) {
      sortedLines foreach write
      close
    }
    tempFile.getAbsolutePath
  }

  def merge(filePaths: String*): String = {
    //TODO: replace first element which is empty. What if file is empty?
    def readNext() = {

    }
    val fileReaders: Seq[Iterator[String]] = filePaths map fileLines
    "TODO"
  }

}

object ExternalSorting {

  def merge() = {
    val heap = PriorityQueue
  }


  implicit class StringBufferOps(values: Buffer[String]) {

    def memorySort: Buffer[String] = {
      def swapElements(first: Int, second: Int): Unit = {
        val element = values(first)
        values(first) = values(second)
        values(second) = element
      }

      def sortRange(left: Int, right: Int): Unit = {
        val pivot = values((left + right) / 2)
        var begin = left
        var end = right
        while (begin <= end) {
          while (values(begin) < pivot) begin += 1
          while (values(end) > pivot) end -= 1
          if (begin <= end) {
            swapElements(begin, end)
            begin += 1
            end -= 1
          }
        }
        if (left < end) sortRange(left, end)
        if (end < right) sortRange(begin, right)
      }
      sortRange(0, values.length - 1)
      values
    }
  }

}