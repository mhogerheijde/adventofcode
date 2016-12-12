package net.hogerheijde.aoc2016.days.day3

import net.hogerheijde.aoc2016.days.day3.model.Triangle
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day3Test extends FlatSpec with Matchers {

  "Day3" should "create a correct Triangle" in {
    Day3.buildTriangle(IndexedSeq(2, 2, 1)) should be (Some(Triangle(1, 2, 2)))
    Day3.buildTriangle(IndexedSeq(8, 5, 4)) should be (Some(Triangle(4, 5, 8)))
  }

  it should "not create a Triangle" in {
    Day3.buildTriangle(IndexedSeq(1, 2, 1)) should be (None)
    Day3.buildTriangle(IndexedSeq(1, 5, 1)) should be (None)
  }

  it should "parse a line correctly" in {
    Day3.parseLine(" 566  477  376") should be (IndexedSeq(566, 477, 376))
    Day3.parseLine("575  488  365 ") should be (IndexedSeq(575, 488, 365))
    Day3.parseLine("  50   18  156 \n") should be (IndexedSeq(50, 18, 156))
    Day3.parseLine("558  673  498") should be (IndexedSeq(558, 673, 498))
    Day3.parseLine("  133  112  510") should be (IndexedSeq(133, 112, 510))
  }

  it should "parse multiple lines correctly" in {
    val input = """
      |  566  477  376
      |  575  488  365
      |   50   18  156
      |  558  673  498
      |  133  112  510
      |
    """.stripMargin


    Day3.parse(input) should be (IndexedSeq(
      IndexedSeq(566, 477, 376),
      IndexedSeq(575, 488, 365),
      IndexedSeq(50, 18, 156),
      IndexedSeq(558, 673, 498),
      IndexedSeq(133, 112, 510)
    ))

  }

}
