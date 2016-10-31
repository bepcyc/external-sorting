package org.viacheslav.rodionov.externalsorting

import java.io.{File, PrintWriter}

import org.scalatest.{Matchers, WordSpec}
import org.viacheslav.rodionov.externalsorting.readers.{FileStringReader, FilesReader}

import scala.collection.{SeqView, mutable}
import scala.language.postfixOps
import scala.util.Random

class ExternalSortingTest extends WordSpec with Matchers {
  val seed = 20161025
  val random = new Random(seed)

  def temporaryFile: File = java.io.File.createTempFile("extsorting", ".txt")

  "Existing file " should {
    "be read according to its capacity" in {
      val contents: Seq[String] = (1 to 7) map { _ => random.nextInt.toString }
      val inputFile: File = temporaryFile
      new PrintWriter(inputFile) {
        write(contents mkString "\n")
        close
      }
      val capacity = 3
      val sorting = ExternalSorting(inputFile.getAbsolutePath, capacity)
      val block1: Iterator[String] = sorting.readIntoMemory
      val part1: Seq[String] = contents take capacity
      block1.toSeq shouldBe part1
      val block2: Iterator[String] = sorting.readIntoMemory
      val part2: Seq[String] = contents.slice(capacity, 2 * capacity)
      block2.toSeq shouldBe part2
      val block3: Iterator[String] = sorting.readIntoMemory
      val part3: Seq[String] = contents.slice(2 * capacity, 3 * capacity)
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

  "Reading lines from a bunch of files" should {
    "be consistent" in {
      val contents: List[List[String]] = List(
        List("aaa", "bbb", "cc"),
        List(),
        List("xxx", "yyy")
      )
      val tempFiles: List[File] = contents map (x => temporaryFile)

      tempFiles.zip(contents) foreach { case (tempFile, content) =>
        new PrintWriter(tempFile) {
          write(content mkString "\n")
          close
        }
      }

      val reader: FilesReader = FilesReader(tempFiles.map(_.getAbsolutePath): _*)
      reader.readLine() shouldBe Some("aaa")
      reader.readLine() shouldBe Some("bbb")
      reader.readLine() shouldBe Some("cc")
      reader.readLine() shouldBe Some("xxx")
      reader.readLine() shouldBe Some("yyy")
      reader.readLine() shouldBe None
      reader.readLine() shouldBe None
    }
  }

  "Corner case for reading zero files" should {
    "not fail" in {
      val zeroReader: FilesReader = FilesReader()
      zeroReader.readLine() shouldBe None
      zeroReader.readLine() shouldBe None
      zeroReader.readLine() shouldBe None
    }
  }

  "Corner case for merging zero files" should {
    "should produce empty file" in {
      val tempFile: File = temporaryFile
      ExternalSorting merge tempFile
      scala.io.Source.fromFile(tempFile).getLines shouldBe empty
    }
  }

  "Corner case for merging one empty file" should {
    "produce empty file" in {
      val inFile: File = temporaryFile
      val outFile: File = temporaryFile
      ExternalSorting.merge(outFile, inFile)
      scala.io.Source.fromFile(outFile).getLines shouldBe empty
    }
  }

  "External sorting" should {
    "work altogether" in {
      val contents: Seq[String] = (1 to 100000) map { x =>
        random
          .alphanumeric
          .take(random.nextInt(10) + 1)
          .mkString
      }
//      val tempFile: File = temporaryFile
      val tempFile: File = new java.io.File("/tmp/file.txt")
      new PrintWriter(tempFile) {
        write(contents mkString "\n")
        close
      }
      val capacity = 300
      val sorting = ExternalSorting(tempFile.getAbsolutePath, capacity)
      val blocks: Seq[mutable.Buffer[String]] =
        0 to Math.ceil(contents.size / capacity).toInt map (_ => sorting.readIntoMemory.toBuffer)
      import ExternalSorting._
      val sortedBlocks: SeqView[mutable.Buffer[String], Seq[_]] = blocks.view map (_.memorySort)
      val tempFiles: Seq[Option[File]] = sortedBlocks map (ExternalSorting saveToFile)
      tempFiles exists (_.isEmpty) shouldBe false
      val outputFile: File = temporaryFile
      ExternalSorting.merge(outputFile, tempFiles.map(_.get): _*)
      scala.io.Source.fromFile(outputFile).getLines.toVector shouldBe contents.sorted
    }
  }


}
