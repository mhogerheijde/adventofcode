
package net.hogerheijde.aoc2020

import fastparse.NoWhitespace._
import fastparse.CharIn
import fastparse.P
import fastparse._
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2021.Day3
import net.hogerheijde.aoc2021.Day4
import net.hogerheijde.aoc2021.Day4.BingoCard
import net.hogerheijde.aoc2021.Day4.BingoSubsystem
import net.hogerheijde.aoc2021.Day4.Cell
import net.hogerheijde.aoc2021.Day4.Coordinate
import net.hogerheijde.aoc2021.Day4.Grid
import net.hogerheijde.aoc2021.Day4.bingoCard
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day4Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        |
        |22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7
        |
        |""".stripMargin
    Day4.parse(input)
  }

  "BingoCard" should {
    "recognise Bingo" in {
      // TRUE
      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(22, true),
        Coordinate(0, 1) -> Cell(13, true),
        Coordinate(1, 0) -> Cell(8),
        Coordinate(1, 1) -> Cell(2),
      )).hasBingo should be(true)

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(22),
        Coordinate(0, 1) -> Cell(13),
        Coordinate(1, 0) -> Cell(8, true),
        Coordinate(1, 1) -> Cell(2, true),
      )).hasBingo should be(true)

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(22),
        Coordinate(0, 1) -> Cell(13, true),
        Coordinate(1, 0) -> Cell(8),
        Coordinate(1, 1) -> Cell(2, true),
      )).hasBingo should be(true)

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(22, true),
        Coordinate(0, 1) -> Cell(13),
        Coordinate(1, 0) -> Cell(8, true),
        Coordinate(1, 1) -> Cell(2),
      )).hasBingo should be(true)

      // FALSE
      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(22, true),
        Coordinate(0, 1) -> Cell(13),
        Coordinate(1, 0) -> Cell(8),
        Coordinate(1, 1) -> Cell(2, true),
      )).hasBingo should be(false)

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(22),
        Coordinate(0, 1) -> Cell(13, true),
        Coordinate(1, 0) -> Cell(8, true),
        Coordinate(1, 1) -> Cell(2),
      )).hasBingo should be(false)
    }

    "mark correct number" in {
      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(1),
        Coordinate(0, 1) -> Cell(2),
        Coordinate(1, 0) -> Cell(3),
        Coordinate(1, 1) -> Cell(4),
      )).mark(1) should be (
        BingoCard(Grid(
          Coordinate(0, 0) -> Cell(1, true),
          Coordinate(0, 1) -> Cell(2),
          Coordinate(1, 0) -> Cell(3),
          Coordinate(1, 1) -> Cell(4),
        ))
      )

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(1),
        Coordinate(0, 1) -> Cell(2),
        Coordinate(1, 0) -> Cell(3),
        Coordinate(1, 1) -> Cell(4),
      )).mark(2) should be(
        BingoCard(Grid(
          Coordinate(0, 0) -> Cell(1),
          Coordinate(0, 1) -> Cell(2, true),
          Coordinate(1, 0) -> Cell(3),
          Coordinate(1, 1) -> Cell(4),
        ))
      )

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(1, true),
        Coordinate(0, 1) -> Cell(2),
        Coordinate(1, 0) -> Cell(3),
        Coordinate(1, 1) -> Cell(4),
      )).mark(2) should be(
        BingoCard(
          Grid(
            Coordinate(0, 0) -> Cell(1, true),
            Coordinate(0, 1) -> Cell(2, true),
            Coordinate(1, 0) -> Cell(3),
            Coordinate(1, 1) -> Cell(4),
          ),
          Some(2)
        )
      )

      BingoCard(Grid(
        Coordinate(0, 0) -> Cell(1, true),
        Coordinate(0, 1) -> Cell(2),
        Coordinate(1, 0) -> Cell(3),
        Coordinate(1, 1) -> Cell(4),
      )).mark(4) should be(
        BingoCard(Grid(
          Coordinate(0, 0) -> Cell(1, true),
          Coordinate(0, 1) -> Cell(2),
          Coordinate(1, 0) -> Cell(3),
          Coordinate(1, 1) -> Cell(4, true),
        ))
      )

    }
    "calculate score" in {
      val card = """14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7
        |
        |""".stripMargin

      val bingoCard = Parser.parse(Day4.bingoCard(_))(card).get
      val marked = Seq(7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24)
        .foldLeft(bingoCard) { case (card, nextDraw) =>
          card.mark(nextDraw)
        }

      marked.calculate should be (4512)

    }

  }

  "Day 4" should {
    "parse a bingo cell" in {
      Parser.parse(Day4.cell(_))("22") should be(Some(Cell(22)))
      Parser.parse(Day4.cell(_))("12") should be(Some(Cell(12)))
      Parser.parse(Day4.cell(_))(" 2") should be(Some(Cell(2)))
      Parser.parse(Day4.cell(_))(" 3") should be(Some(Cell(3)))
    }

    "parse a bingo-line" in {
      val card =
        """22 13 15 46
          |""".stripMargin

      Parser.parse(Day4.bingoLine(_))(card) should be(Some(
        Seq(
          Cell(22),
          Cell(13),
          Cell(15),
          Cell(46),
        )
      ))
    }

    "parse a bingo-card" in {
      val card =
        """22 13
          | 8  2
          |
          |""".stripMargin

      Parser.parse(Day4.bingoCard(_))(card) should be(Some(
        BingoCard(Grid(
          Coordinate(0, 0) -> Cell(22),
          Coordinate(0, 1) -> Cell(13),
          Coordinate(1, 0) -> Cell(8),
          Coordinate(1, 1) -> Cell(2),
        ))
      ))
    }


    "parse two bingo-cards" in {
      val card =
        """22 13
          | 8  2
          |
          | 3 15
          | 9 18
          |
          |""".stripMargin

      Parser.parse(Day4.bingoCards(_))(card).get should be(Seq(
        BingoCard(Grid(
          Coordinate(0, 0) -> Cell(22),
          Coordinate(0, 1) -> Cell(13),
          Coordinate(1, 0) -> Cell(8),
          Coordinate(1, 1) -> Cell(2),
        )),
        BingoCard(Grid(
          Coordinate(0, 0) -> Cell(3),
          Coordinate(0, 1) -> Cell(15),
          Coordinate(1, 0) -> Cell(9),
          Coordinate(1, 1) -> Cell(18),
        ))
      ))
    }

    "parse subsystem" in {
      exampleInput.draw should be (Seq(
        7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1
      ))
      exampleInput.cards should have size(3)
    }

    "calc part 1" in {
      Day4.part1(exampleInput) should be (4512)
    }

    "calc part 2" in {
      Day4.part2(exampleInput) should be(1924)
    }
  }

}
