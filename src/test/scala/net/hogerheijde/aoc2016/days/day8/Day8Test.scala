package net.hogerheijde.aoc2016.days.day8

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day8Test extends FlatSpec with Matchers {

  "Day8" should "parse rect command correctly" in {

    Day8.parseCommand("rect 1x1") should be(Some(Rect(1, 1)))
    Day8.parseCommand("rect 2x1") should be(Some(Rect(2, 1)))
    Day8.parseCommand("rect 3x1") should be(Some(Rect(3, 1)))
    Day8.parseCommand("rect 40x1") should be(Some(Rect(40, 1)))
    Day8.parseCommand("rect 1x2") should be(Some(Rect(1, 2)))
    Day8.parseCommand("rect 1x3") should be(Some(Rect(1, 3)))
    Day8.parseCommand("rect 1x40") should be(Some(Rect(1, 40)))

  }

  it should "parse rotate column command correctly" in {
    Day8.parseCommand("rotate column x=0 by 1") should be(Some(RotateColumn(0, 1)))
    Day8.parseCommand("rotate column x=0 by 2") should be(Some(RotateColumn(0, 2)))
    Day8.parseCommand("rotate column x=1 by 1") should be(Some(RotateColumn(1, 1)))
    Day8.parseCommand("rotate column x=10 by 20") should be(Some(RotateColumn(10, 20)))
    Day8.parseCommand("rotate column x=25 by 3") should be (Some(RotateColumn(25, 3)))
  }

  it should "parse rotate row command correctly" in {
    Day8.parseCommand("rotate row y=0 by 1") should be(Some(RotateRow(0, 1)))
    Day8.parseCommand("rotate row y=0 by 2") should be(Some(RotateRow(0, 2)))
    Day8.parseCommand("rotate row y=1 by 1") should be(Some(RotateRow(1, 1)))
    Day8.parseCommand("rotate row y=10 by 20") should be(Some(RotateRow(10, 20)))
  }

  it should "parse a list of command correctly" in {
    val input = """rect 2x1
                  |rotate row y=0 by 5
                  |rotate column x=0 by 1""".stripMargin

    Day8.parse(input) should be(IndexedSeq(
      Rect(2, 1),
      RotateRow(0, 5),
      RotateColumn(0, 1)
    ))
  }

}
