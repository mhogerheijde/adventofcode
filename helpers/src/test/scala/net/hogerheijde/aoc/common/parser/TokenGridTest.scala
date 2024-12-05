package net.hogerheijde.aoc.common.parser

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.model.Coordinate
import net.hogerheijde.aoc.common.model.Grid
import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TokenGridTest extends AnyWordSpec with Matchers:

  "TokenGrid" should {
    "parse char grid" in {
      val input =
        """abcdef
          |ghijkl
          |mnopqr""".stripMargin

      Parser.parse(TokenGrid.charGrid(_))(input).get should be (
        Grid[Char](
          (Coordinate(0, 0), 'a'),
          (Coordinate(0, 1), 'b'),
          (Coordinate(0, 2), 'c'),
          (Coordinate(0, 3), 'd'),
          (Coordinate(0, 4), 'e'),
          (Coordinate(0, 5), 'f'),
          (Coordinate(1, 0), 'g'),
          (Coordinate(1, 1), 'h'),
          (Coordinate(1, 2), 'i'),
          (Coordinate(1, 3), 'j'),
          (Coordinate(1, 4), 'k'),
          (Coordinate(1, 5), 'l'),
          (Coordinate(2, 0), 'm'),
          (Coordinate(2, 1), 'n'),
          (Coordinate(2, 2), 'o'),
          (Coordinate(2, 3), 'p'),
          (Coordinate(2, 4), 'q'),
          (Coordinate(2, 5), 'r'),
        )
      )
    }
  }
