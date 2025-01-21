package net.hogerheijde.aoc2024

import fastparse.*
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc.text.AnsiHelpers.AnsiString
import net.hogerheijde.aoc2024.Day9.Block.FileParts
import net.hogerheijde.aoc2024.Day9.Block.FileParts
import net.hogerheijde.aoc2024.Day9.Block.Space
import net.hogerheijde.aoc2024.Day9.Disk
import net.hogerheijde.aoc2024.Day9.Identifier
import net.hogerheijde.aoc2024.Day9.defragStrategy1
import net.hogerheijde.aoc2024.Day9.isCompacted1
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    """2333133121414131402
      |""".stripMargin


  "Day 9 parser" should {
    "parse" in {
       Parser.parse(Day9.disk(_))("12345").get  should be (
         Disk(IndexedSeq(
           FileParts(Identifier(0), 1),
           Space(2),
           FileParts(Identifier(1), 3),
           Space(4),
           FileParts(Identifier(2), 5),
         ))
       )
    }
  }

  "Disk" should {
    "defrag step 12345" in {
      Day9.parse("12345").defrag1Step().map should be (
        IndexedSeq(
          FileParts(Identifier(0), 1),
          FileParts(Identifier(2), 2, false),
          FileParts(Identifier(1), 3),
          FileParts(Identifier(2), 3),
        )
      )
      Day9.parse("12345").defrag1Step().toString should be ("0[22]111222")
    }

    "defrag 12345" in {
      Day9.parse("12345").defrag(defragStrategy1, isCompacted1).toString should be("0[22]111222")
    }

    "defrag step example" in {
      val disk0 = Day9.parse(exampleInput)
      val disk1 = disk0.defrag1Step()
      disk1.toString should be(
        "0099.111...2...333.44.5555.6666.777.8888"
      )

      val disk2 = disk1.defrag1Step()
      disk2.toString should be(
        "0099[8]1118882...333.44.5555.6666.777"
      )

      val disk3 = disk2.defrag1Step()
      disk3.toString should be(
        "0099[8]1118882777333.44.5555.6666"
      )

      val disk4 = disk3.defrag1Step()
      disk4.toString should be(
        "0099[8]1118882777333[6]44[6]5555[6]6"
      )
    }

    "defrag2 step example" in {
      val disk0 = Day9.parse(exampleInput)
      val disk1 = disk0.defrag2Step()
      disk1.toString should be(
        "00[99].111...2...333.44.5555.6666.777.8888.."
      )

      val disk2 = disk1.defrag2Step()
      disk2.toString should be(
        "00[99].111...2...333.44.5555.6666.777.[8888].."
      )

      val disk3 = disk2.defrag2Step()
      disk3.toString should be(
        "00[99].111[777]2...333.44.5555.6666.....[8888].."
      )

      val disk4 = disk3.defrag2Step()
      disk4.toString should be(
        "00[99].111[777]2...333.44.5555.[6666].....[8888].."
      )
    }


    "defrag example 1" in {
      Day9.parse(exampleInput).defrag(defragStrategy1, isCompacted1).toString should be(
        "0099[8]1118882777333[6]44[6]5555[6]6"
      )
    }
  }

  "Day 9" should {

    "parse input" in {
      Day9.parse(exampleInput).toString should be(
        "00...111...2...333.44.5555.6666.777.888899"
      )
    }

    "Part1: example answer" in {
      Day9.part1(Day9.parse(exampleInput)) should be(1928)
    }

    "Part2: example answer" in {
      Day9.part2(Day9.parse(exampleInput)) should be(2858)
    }
  }
}