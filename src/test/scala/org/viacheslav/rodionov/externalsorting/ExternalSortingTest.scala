package org.viacheslav.rodionov.externalsorting

import java.io.PrintWriter

import org.scalatest.{Matchers, WordSpec}
import org.viacheslav.rodionov.ExternalSorting

import scala.Seq
import scala.collection.mutable
import scala.util.Random

class ExternalSortingTest extends WordSpec with Matchers {
  val seed = 2016
  val random = new Random(seed)

  "Existing file " should {
    "be read according to its capacity" in {
      val contents: Seq[String] = (1 to 7) map { _ => random.nextInt.toString }
      val tempFile = java.io.File.createTempFile("extsorting", ".txt")
      new PrintWriter(tempFile) {
        write(contents mkString "\n")
        close
      }
      val capacity = 3
      val sorting = ExternalSorting(tempFile.getAbsolutePath, capacity)
      val block1: Iterator[String] = sorting.readIntoMemory
      val part1: Seq[String] = contents take capacity
      block1.toSeq shouldBe part1
      val block2: Iterator[String] = sorting.readIntoMemory
      val part2: Seq[String] = contents drop capacity take capacity
      block2.toSeq shouldBe part2
      val block3: Iterator[String] = sorting.readIntoMemory
      val part3: Seq[String] = contents drop 2 * capacity take capacity
      block3.toSeq shouldBe part3
    }
  }

  "Sorting algorithm" should {
    "work correctly" in {
      val list: Seq[String] = (1 to 10) map { _ => random.nextInt.toString }
      val listSorted: Seq[String] = list.sorted
      val buffer: mutable.Buffer[String] = list.toBuffer
      import ExternalSorting._
      buffer.memorySort shouldBe listSorted
    }
  }


}
