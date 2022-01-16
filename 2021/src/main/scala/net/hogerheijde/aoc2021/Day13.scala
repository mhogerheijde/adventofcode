package net.hogerheijde.aoc2021

import fastparse._
import fastparse.NoWhitespace._
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.parser.Common
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2021.Day13.Fold.Horizontal
import net.hogerheijde.aoc2021.Day13.Fold.Vertical

object Day13 extends Day[Int, String]{
  type Model = Folding

  case class Folding(
      dots: Set[Coordinate],
      folds: Seq[Fold],
  ) {

    def fold: Option[Folding] = {
      folds match {
        case Seq() => None
        case Seq(lastFold) => Some(Folding(fold(lastFold), Seq()))
        case nextFold +: folds => Some(Folding(fold(nextFold), folds))
      }
    }

    private def fold(f: Fold): Set[Coordinate] = {
      f match {
        case Horizontal(foldLine) =>
          dots.map {
            case Coordinate(x, y) if y > foldLine => Coordinate(x, foldLine - Math.abs(y - foldLine))
            case c => c
          }
        case Vertical(foldLine) =>
          dots.map {
            case Coordinate(x, y) if x > foldLine => Coordinate(foldLine - Math.abs(x - foldLine), y)
            case c => c
          }
      }

    }

    override def toString: String = {
      val sb = new StringBuilder
      var maxY = dots.maxBy(_.y).y
      var maxX = dots.maxBy(_.x).x

      Range.inclusive(0, maxY).foreach { y =>
        Range.inclusive(0, maxX).foreach { x =>
          if (dots.contains(Coordinate(x, y))) {
            sb.append("#")
          } else {
            sb.append(".")
          }
        }
        sb.append("\n")
      }
      sb.toString()
    }
  }

  sealed trait Fold
  object Fold {
    case class Horizontal(y: Int) extends Fold
    case class Vertical(x: Int) extends Fold
  }

  def horizontal[_: P]: P[Fold] = P("fold along y=" ~ Common.int)
    .map { horizontal => Fold.Horizontal(y = horizontal) }
  def vertical[_: P]: P[Fold] = P("fold along x=" ~ Common.int)
    .map { vertical => Fold.Vertical(x = vertical) }
  def fold[_: P]: P[Fold] = P(horizontal | vertical)
  def folding[_: P]: P[Folding] = P(Common.coordinates ~ "\n" ~ (fold ~ "\n").rep)
    .map { case (c, f) => Folding(c.toSet, f) }

  override def parse(input: String): Model = Parser.parse(folding(_))(input).get
  override def part1(input: Model): Int = input.fold.get.dots.size
  override def part2(input: Model): String = {
    val result = LazyList
      .unfold(input) { _.fold.map(folding => (folding, folding)) }
      .last
    "\n" + result.toString
  }
}
