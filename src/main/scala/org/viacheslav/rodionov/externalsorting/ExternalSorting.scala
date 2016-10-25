package org.viacheslav.rodionov.externalsorting

import java.io.PrintWriter

import org.viacheslav.rodionov.externalsorting.readers.FileStringReader

import scala.collection.mutable
import scala.collection.mutable.{Buffer, PriorityQueue}

case class ExternalSorting(inputFile: String, capacity: Int) extends ExternalAccess {
  lazy private val lines: Iterator[String] = fileLines(inputFile)

  def readIntoMemory: Iterator[String] = lines take capacity

}

object ExternalSorting extends ExternalAccess {

  def saveToFile(sortedLines: mutable.Buffer[String]): String = {
    val tempFile = java.io.File.createTempFile("extsrttbl", ".txt")
    new PrintWriter(tempFile) {
      sortedLines foreach { line => write(line + "\n") }
      close
    }
    tempFile.getAbsolutePath
  }

  def merge(outFilePath: String, readers: FileStringReader*): Unit = {
    implicit val ordering: Ordering[FileStringReader] = FileStringReader.ord
    val heap: mutable.PriorityQueue[FileStringReader] = mutable.PriorityQueue[FileStringReader]()
    heap enqueue (readers: _*)
    new PrintWriter(outFilePath) {
      while (heap.nonEmpty) {
        val minimal: FileStringReader = heap dequeue()
        minimal.currentHeadOption match {
          case Some(str) => {
            write(str + "\n")
            minimal next() match {
              case None => {
                // we're done with this reader
              }
              case Some(nextStr) => heap enqueue minimal
            }
          }
          case None => {
            // we're done with this reader
          }
        }
      }
      close
    }
  }

  implicit class StringBufferOps(values: mutable.Buffer[String]) {

    def memorySort: mutable.Buffer[String] = values.synchronized {
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