package net.hogerheijde.aoc2017

import net.hogerheijde.aoc2017.day2.Cell
import net.hogerheijde.aoc2017.day2.Day2
import net.hogerheijde.aoc2017.day2.Row
import net.hogerheijde.aoc2017.day2.Sheet
import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day2Test extends WordSpec with Matchers {

  private val example =
    """
      |5	1	9	5
      |7	5	3
      |2	4	6	8
      |""".stripMargin.trim

  private val exampleSheet = Sheet(
    Row(5, 1, 9, 5),
    Row(7, 5, 3),
    Row(2, 4, 6, 8)
  )

  private val exampleSheet2 = Sheet(
    Row(5, 9, 2, 8),
    Row(9, 4, 7, 3),
    Row(3, 8, 6, 5)
  )


  "Day2" should {
    "parse" in {
      Day2.parse(example) should be (exampleSheet)
    }
  }

  "Cell" should {
    "construct from string" in {
      Cell.fromString("123") should contain(Cell(123))
      Cell.fromString(" 321 ") should contain(Cell(321))
    }
  }

  "Row" should {
    "construct from string" in {
      Row.fromLine("1\t2\t3") should be(Row(1, 2, 3))
    }

    "calculate checksum" in {
      Row(5, 1, 9, 5).checksum should be(8)
      Row(7, 5, 3).checksum should be(4)
      Row(2, 4, 6, 8).checksum should be(6)
    }

    "find division" in {
      Row(5, 9, 2, 8).division should contain(4)
      Row(9, 4, 7, 3).division should contain(3)
      Row(3, 8, 6, 5).division should contain(2)
    }
  }


  "Sheet" should {
    "calculate checksum" in {
      exampleSheet.checksum should be (18)
    }

    "find division checksum" in {
      exampleSheet2.checksum2 should be (9)
    }
  }
}
