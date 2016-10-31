package org.viacheslav.rodionov.externalsorting

import java.io.File

import org.viacheslav.rodionov.externalsorting.readers.FileStringReader

object Demo {

  def main(args: Array[String]): Unit = {
    if (args.size != 2) {
      println("Wrong number of arguments!")
      println("Usage: scala org.viacheslav.rodionov.externalsorting.Demo <input-file-name> <output-file-name>")
    } else {
      val inputFilePath = args.head
      val outputFilePath = args.tail.head
      val inputFile: File = new java.io.File(inputFilePath)
      val outputFile: File = new java.io.File(outputFilePath)
      val capacity = 10000
      val sorting = ExternalSorting(inputFile, capacity)
      val intermediateFiles =
        Stream from 1 map (x => sorting.writeNextSortedBlock) takeWhile (_.isDefined) map (_.get) 
      ExternalSorting.merge(outputFile, intermediateFiles: _*)
      println(s"Resulting sorted list is written into a file at $outputFilePath")
    }
  }
}
