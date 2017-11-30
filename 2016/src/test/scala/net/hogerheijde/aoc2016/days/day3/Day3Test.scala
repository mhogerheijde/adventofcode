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


    Day3.parseRows(input) should be (IndexedSeq(
      IndexedSeq(566, 477, 376),
      IndexedSeq(575, 488, 365),
      IndexedSeq(50, 18, 156),
      IndexedSeq(558, 673, 498),
      IndexedSeq(133, 112, 510)
    ))

  }

  it should "parse columns correctly" in {
    val input = """
      |  101 301 501
      |  102 302 502
      |  103 303 503
      |  201 401 601
      |  202 402 602
      |  203 403 603
      |
    """.stripMargin

    Day3.parseColumns(input) should be (IndexedSeq(
      IndexedSeq(101, 102, 103),
      IndexedSeq(301, 302, 303),
      IndexedSeq(501, 502, 503),
      IndexedSeq(201, 202, 203),
      IndexedSeq(401, 402, 403),
      IndexedSeq(601, 602, 603)
    ))

  }

  it should "create correct triangles for columns" in {
    val input = """
      |  102 302 502
      |  102 302 502
      |  103 303 503
      |  202 402 602
      |  202 402 602
      |  203 403 603
      |
    """.stripMargin

    Day3.processAsColumns(input) should be (IndexedSeq(
      Triangle(102, 102, 103),
      Triangle(302, 302, 303),
      Triangle(502, 502, 503),
      Triangle(202, 202, 203),
      Triangle(402, 402, 403),
      Triangle(602, 602, 603)
    ))

  }

}
