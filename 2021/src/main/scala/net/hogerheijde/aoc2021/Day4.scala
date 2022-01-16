package net.hogerheijde.aoc2021

import fastparse.NoWhitespace._
import fastparse.P
import fastparse._
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.common.parser.Common
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc.text.AnsiHelpers._

object Day4 extends Day[Int, Int] {

  case class BingoSubsystem(
      draw: IndexedSeq[Int],
      cards: Seq[BingoCard],
      current: Int = 0,
  ) {
    def iterator: Iterator[BingoSubsystem] = new BingoSubsystem.BingoIterator(this)

    private def drawNext: BingoSubsystem = {
      BingoSubsystem(
        draw,
        cards.map(_.mark(draw(current))),
        current + 1
      )
    }

    def bingo: Seq[BingoCard] = cards.filter(_.hasBingo)
  }

  object BingoSubsystem {
    class BingoIterator(bingoSubsystem: BingoSubsystem) extends Iterator[BingoSubsystem] {
      private var subsystem = bingoSubsystem

      override def hasNext: Boolean = bingoSubsystem.draw.size <= bingoSubsystem.current

      override def next(): BingoSubsystem = {
        val next = subsystem.drawNext
        subsystem = next
        next
      }
    }
  }

  case class Cell(
      value: Int,
      marked: Boolean = false,
  ) {
    def mark: Cell = Cell(value, true)

    override def toString: String = if (marked) {
      value.toString.reverse.padTo(2, ' ').reverse.bold.red
    } else {
      value.toString.reverse.padTo(2, ' ').reverse
    }
  }

//  type Grid = Map[Coordinate, Cell]
//  val Grid = Map

  case class BingoCard(
      cells: Grid[Cell],
      bingoDraw: Option[Int] = None,
  ) {
    def mark(draw: Int): BingoCard = {
      if (hasBingo) { this
      } else {
        val card = this.copy(cells =
          Grid(cells
            .values
            .view
            .mapValues { cells =>
              if (cells.value == draw) {
                cells.mark
              } else {
                cells
              }
            }
            .toMap)
        )
        if (card.calcBingo && card.bingoDraw.isEmpty) {
          card.copy(bingoDraw = Some(draw))
        } else {
          card
        }
      }
    }

    val rows: Map[Int, Iterable[Cell]] = cells
      .values
      .groupBy { case (Coordinate(row, _), _) => row }
      .view
      .mapValues {
        _.values
      }
      .toMap
    val columns = cells
      .values
      .groupBy { case (Coordinate(_, column), _) => column }
      .view
      .mapValues {
        _.values
      }
      .toMap

    val hasBingo = bingoDraw.nonEmpty

    private def calcBingo: Boolean = rows.exists { case (_, row) => row.forall(_.marked) } ||
      columns.exists { case (_, column) => column.forall(_.marked) }

    def calculate: Int = {
      bingoDraw.map(_ * cells.values.values.filterNot(_.marked).map(_.value).sum).getOrElse(0)
    }

    override def toString: String = {
      val bingo = if (hasBingo) {
        s"* BINGO * (${bingoDraw.get}) \n"
      } else {
        ""
      }
      bingo +
        rows
          .map { case (_, cells) => cells.mkString(" ") }
          .mkString("\n") + "\n"
    }
  }

  object BingoCard {
    def apply(cells: Map[Coordinate, Cell]): BingoCard = BingoCard(Grid(cells))
  }

  def draw[_: P]: P[IndexedSeq[Int]] = P(Common.intSeq ~ "\n")

  def cell[_: P]: P[Cell] = P((CharIn(" 0-9") ~ CharIn("0-9")).!).map { value => Cell(value.trim.toInt) }

  def bingoLine[_: P]: P[Seq[Cell]] = P((cell ~ " ".?).rep(1) ~ "\n")

  def bingoCard[_: P]: P[BingoCard] = P(bingoLine.rep(1) ~ "\n").map { q =>
    val z = q.foldLeft((0, Map.empty[Coordinate, Cell])) { case ((row, acc), nextLine) =>
      val rowOfCells = nextLine.zipWithIndex.map { case (cell, column) =>
        (Coordinate(column, row), cell)
      }.toMap
      (row + 1, acc ++ rowOfCells)
    }
    BingoCard(z._2)
  }

  def bingoCards[_: P]: P[Seq[BingoCard]] = P(bingoCard.rep)

  def bingoSubsystem[_: P]: P[BingoSubsystem] = P(draw ~ "\n" ~ bingoCards).map { case (draw, cards) =>
    BingoSubsystem(draw, cards)
  }

  override type Model = BingoSubsystem

  override def parse(input: String): Model = Parser.parse(bingoSubsystem(_))(input).get

  override def part1(input: Model): Int = {
    val iterator = input.iterator
    val bingo = LazyList.continually(iterator.next()).find { q => q.bingo.nonEmpty }.get
    bingo.bingo.head.calculate
  }

  override def part2(input: Model): Int = {
    val fullDraw = input.draw.foldLeft(input) { case (acc, nextDraw) =>
      val cs = acc.cards.map(_.mark(nextDraw))
//      println(s"Drawing $nextDraw")
//      println(cs.mkString("\n"))
      BingoSubsystem(
        input.draw,
        cs,
        0
      )
    }
    val bingoCards = fullDraw.bingo.map { card =>
      (card, input.draw.indexOf(card.bingoDraw.get))
    }.sortBy(_._2)

    println(bingoCards.mkString("\n\n"))

    val lastCardToWin = bingoCards.reverse.head
    lastCardToWin._1.calculate

  }
}
