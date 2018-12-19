package net.hogerheijde.aoc2018.day13

import scala.annotation.tailrec

import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.util.Implicits._

// scalastyle:off


object Model {

  sealed trait TrackType
  case object Space extends TrackType { override def toString: String = " " }
  case object Horizontal extends TrackType { override def toString: String = "─" }
  case object Vertical extends TrackType { override def toString: String = "│" }
  case object TopLeft extends TrackType { override def toString: String = "┘" }
  case object TopRight extends TrackType { override def toString: String = "└" }
  case object BottomLeft extends TrackType { override def toString: String = "┐" }
  case object BottomRight extends TrackType { override def toString: String = "┌" }
  case object Crossing extends TrackType { override def toString: String = "┼" }

  object TrackType {

    private val HorizontalChars = Set('-', '+', '>', '<')
    def isHorizontal(c: Char): Boolean = HorizontalChars.contains(c)

    def forChar(prev: Char, c: Char, next: Char, coordinate: Coordinate): (TrackType, Option[Cart]) = {
      c match {
        case ' ' => (Space, None)
        case '-' => (Horizontal, None)
        case '|' => (Vertical, None)
        case '/' => if (isHorizontal(next)) { (BottomRight, None) } else { (TopLeft, None) }
        case '\\' => if (isHorizontal(prev)) { (BottomLeft, None) } else { (TopRight, None) }
        case '+' => (Crossing, None)

        case '^' => (Vertical, Some(Cart(coordinate, North, Right)))
        case 'v' => (Vertical, Some(Cart(coordinate, South, Right)))
        case '>' => (Horizontal, Some(Cart(coordinate, East, Right)))
        case '<' => (Horizontal, Some(Cart(coordinate, West, Right)))

        case _ => (Space, None) // Treat everything we don't know as a space
      }
    }

  }


  sealed trait Direction {
    def rotateLeft: Direction
    def rotateRight: Direction
  }

  case object North extends Direction {
    val rotateLeft = West
    val rotateRight = East
    override def toString: String = "^"
  }
  case object South extends Direction {
    val rotateLeft = East
    val rotateRight = West
    override def toString: String = "v"
  }
  case object East extends Direction {
    val rotateLeft = North
    val rotateRight = South
    override def toString: String = ">"
  }
  case object West extends Direction {
    val rotateLeft = South
    val rotateRight = North
    override def toString: String = "<"
  }


  sealed trait Choice {
    def next: Choice
  }
  case object Left extends Choice { val next = Straight}
  case object Right extends Choice { val next = Left }
  case object Straight extends Choice { val next = Right }

  case class Cart(coordinate: Coordinate, direction: Direction, lastChoice: Choice) {
    val nextCoordinate: Coordinate = direction match {
      case North => coordinate.copy(y  = coordinate.y - 1)
      case South => coordinate.copy(y  = coordinate.y + 1)
      case West => coordinate.copy(x  = coordinate.x - 1)
      case East => coordinate.copy(x  = coordinate.x + 1)
    }

  }

  case class Grid(tracks: Map[Coordinate, TrackType], carts: Map[Coordinate, Cart]) {

    @tailrec
    final def lastRemaining(i: Int = 0): Coordinate = {
      if (carts.size == 1) {
        println(s"Found in $i generations")
        carts.head._1
      }
      else {
        safeNext.lastRemaining(i + 1)
      }
    }

    @tailrec
    final def collision: Coordinate = {
      this.next match {
        case scala.util.Left(grid) => grid.collision
        case scala.util.Right(collision) => collision
      }
    }

    val sortedCarts = carts.values.toSeq.sortBy { c =>
      (c.coordinate.y, c.coordinate.x)
    }
    def maxX = tracks.keySet.maxBy(_.x).x
    def maxY = tracks.keySet.maxBy(_.y).y

    def safeNext: Grid = {
      sortedCarts.foldLeft(this) { (grid, cart) =>

        if (!grid.carts.contains(cart.coordinate)) {
          println("Working on cart that was removed?! Continue!")
          grid
        } else {

          val track = tracks(cart.nextCoordinate)

          if (track == Space) {
            throw new RuntimeException(s"We've got a derailing for ${cart}! \n\n ${this.toString}")
          }

          val (direction, choice) = track match {
            case Crossing => {
              cart.lastChoice.next match {
                case Left => (cart.direction.rotateLeft, Left)
                case Right => (cart.direction.rotateRight, Right)
                case Straight => (cart.direction, Straight)
              }
            }
            case other => {
              val r = other match {
                case TopLeft => if (cart.direction == South) {
                  West
                } else {
                  North
                }
                case TopRight => if (cart.direction == South) {
                  East
                } else {
                  North
                }
                case BottomLeft => if (cart.direction == North) {
                  West
                } else {
                  South
                }
                case BottomRight => if (cart.direction == North) {
                  East
                } else {
                  South
                }
                case _ => cart.direction
              }
              (r, cart.lastChoice)
            }

          }

          grid.carts.get(cart.nextCoordinate) match {
            case Some(_) =>
              val newCarts2 = grid.carts - cart.coordinate - cart.nextCoordinate
              grid.copy(carts = newCarts2)
            case None =>
              val newCart = Cart(cart.nextCoordinate, direction, choice)
              val newCarts = grid.carts.updated(cart.nextCoordinate, newCart)
              val newCarts2 = newCarts - cart.coordinate
              grid.copy(carts = newCarts2)
          }
        }
      }
    }

    def next: Either[Grid, Coordinate] = {
      sortedCarts.foldLeft(scala.util.Left(this): scala.util.Either[Grid, Coordinate]) { (either, cart) =>

        either match {
          case scala.util.Right(collision) => scala.util.Right(collision)
          case scala.util.Left(grid) => {

            val track = tracks(cart.nextCoordinate) // Assume grid is correct and next track exists

            if (track == Space) {
              throw new RuntimeException(s"We've got a derailing for ${cart}! \n\n ${this.toString}")
            }

            val (direction, choice) = track match {
              case Crossing => {
                cart.lastChoice.next match {
                  case Left => (cart.direction.rotateLeft, Left)
                  case Right => (cart.direction.rotateRight, Right)
                  case Straight => (cart.direction, Straight)
                }
              }
              case other => {
                val r = other match {
                  case TopLeft => if (cart.direction == South) { West } else { North }
                  case TopRight => if (cart.direction == South) { East } else { North }
                  case BottomLeft => if (cart.direction == North) { West } else { South }
                  case BottomRight => if (cart.direction == North) { East} else { South }
                  case _ => cart.direction
                }
                (r, cart.lastChoice)
              }

            }

            grid.carts.get(cart.nextCoordinate) match {
              case Some(_) => scala.util.Right(cart.nextCoordinate)
              case None =>
                val newCart = Cart(cart.nextCoordinate, direction, choice)
                val newCarts = grid.carts.updated(cart.nextCoordinate, newCart)
                val newCarts2 = newCarts - cart.coordinate
                scala.util.Left(grid.copy(carts = newCarts2))
            }
          }
        }
      }
    }


    override def toString: String = {
      val b = new StringBuilder
      Range.inclusive(0, maxY).foreach { line =>
        Range.inclusive(0, maxX).foreach { x =>
          val coordinate = Coordinate(x, line)
          val charToDraw = {
            carts.get(coordinate) match {
              case Some(cart) => cart.direction.toString.red
              case None => tracks.get(coordinate).map(_.toString).getOrElse(" ")
            }
          }
          b append charToDraw
        }
        b append "\n"
      }
      b.toString
    }
  }

  object Grid {
    def parse(input: String): Grid = {
      val tracksWithCarts = input.lines.toIndexedSeq.map(l => s" $l ").zipWithIndex.flatMap {case (line, y) =>
        line
          .sliding(3)
          .zipWithIndex
          .map { case (segment, x) =>
            val c = Coordinate(x, y)
            c -> TrackType.forChar(segment(0), segment(1), segment(2), c)
          }

      }

      val tracks = tracksWithCarts.groupByFirstUnique.mapValues(_._1).withDefaultValue(Space)
      val carts = tracksWithCarts.filter(_._2._2.isDefined).flatMap(_._2._2).groupByUnique(_.coordinate)


      Grid(tracks, carts)
    }
  }

}
