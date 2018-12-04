package net.hogerheijde.aoc2018.day3


import fastparse._, NoWhitespace._

object Model {

  case class Fabric(squares: Set[Square]) {

//    private val upperLeft: Point = squares.map(_.leftTop).min
    private val mostDown: Int = squares.map(_.rightBottom.y).max
    private val mostRight: Int = squares.map(_.rightBottom.x).max

    override def toString: String = {
      Range.inclusive(1, mostDown + 1).map { line =>
        // For every line
        Range.inclusive(1, mostRight + 1).foldLeft("") { case (resultString, pos) =>

          val poitnToCheckFor = Point(pos, line)
          val s = squares.filter(s => s.contains(poitnToCheckFor)).take(2).toSeq
          val charToPrint = s match {
            case Seq() => "."
            case Seq(single) => (single.id % 10).toString
            case _ => "X"
          }
          resultString + charToPrint
        }

      }.mkString("\n")
    }

  }

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    def isLeftOf(other: Point): Boolean = this.x < other.x
    def isRightOf(other: Point): Boolean = this.x >= other.x
    def isAbove(other: Point): Boolean = this.y < other.y
    def isBelow(other: Point): Boolean = this.y >= other.y

    override def compare(that: Point): Int = {
      // Y biased, so that you can order in lines from top to bottom.
      val y = this.y.compare(that.y)
      if (y == 0) this.x.compare(that.x) else y
    }
  }
  case class Square(id: Int, leftTop: Point, rightBottom: Point) extends Ordered[Square] {

    def isAbove(other: Square): Boolean = this.rightBottom.isAbove(other.leftTop)
    def isBelow(other: Square): Boolean = this.leftTop.isBelow(other.rightBottom)
    def isLeftOf(other: Square): Boolean = this.rightBottom.isLeftOf(other.leftTop)
    def isRightOf(other: Square): Boolean = this.leftTop.isRightOf(other.rightBottom)


    def doesNotIntersect(other: Square): Boolean = {
      isLeftOf(other) || isRightOf(other) || isAbove(other) || isBelow(other)
    }
    def intersects(other: Square): Boolean = {
      !doesNotIntersect(other)
    }

    def contains(p: Point): Boolean = {
      this.leftTop.isLeftOf(p) &&
        this.leftTop.isAbove(p) &&
        this.rightBottom.isRightOf(p) &&
        this.rightBottom.isBelow(p)
    }

    override def compare(that: Square): Int = this.leftTop.compare(that.leftTop)
  }

  object Square {
    def parse(input:String): Option[Square] = {
      fastparse.parse(input, parseSquare(_)) match {
        case Parsed.Success(square, _) => Some(square)
        case Parsed.Failure(_, _, e) =>
          val t = e.trace()
          println(t)
          None
      }
    }
  }



  private def parseInt[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  private def parseCoordinates[_: P]: P[Point] = P((parseInt ~ "," ~ parseInt).map(t => Point(t._1, t._2)))
  private def parseSize[_: P]: P[(Int, Int)] = P(parseInt ~ "x" ~ parseInt)

  private def parseSquare[_: P]: P[Square] = P("#" ~ parseInt ~ " @ " ~ parseCoordinates ~ ": " ~ parseSize)
    .map { case (id, point, (sizeX, sizeY)) =>
      Square(id, point, Point(point.x + sizeX, point.y + sizeY))
    }

}
