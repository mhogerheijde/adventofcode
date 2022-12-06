package net.hogerheijde.aoc2022

import scala.util.chaining.scalaUtilChainingOps

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2022.Day5._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Test extends AnyWordSpec with Matchers {
  
  val exampleInput =
    """    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2
      |""".stripMargin

  val exampleStack = IndexedSeq(
    CargoStack('N', 'Z'),
    CargoStack('D', 'C', 'M'),
    CargoStack('P'),
  )

  val exampleMoves = List(
    Move(1, 2, 1),
    Move(3, 1, 3),
    Move(2, 2, 1),
    Move(1, 1, 2),
  )

  val exampleModel = State(
    exampleStack,
    exampleMoves,
  )

  "Day 5 parser" should {

    "parse stackitem" in {
      Parser.parse(stackItem(_))("   ").get should be (None)
//      Parser.parse(stackItem(_))("    ").get should be (None)
      Parser.parse(stackItem(_))("[A]").get should be (Some('A'))
      Parser.parse(stackItem(_))("[B] ").get should be (Some('B'))
      Parser.parse(stackItem(_))("[Z]").get should be (Some('Z'))
    }
    "parse line" in {
      Parser.parse(stackLine(_))("    [D]    \n").get should be(Seq(None, Some('D'), None))
      Parser.parse(stackLine(_))("[N] [C]    \n").get should be(Seq(Some('N'), Some('C'), None))
      Parser.parse(stackLine(_))("[Z] [M] [P]\n").get should be(Seq(Some('Z'), Some('M'), Some('P')))
    }

    "parse stackLines" in {
      val in = 
        """    [D]    
          |[N] [C]    
          |[Z] [M] [P]
          |""".stripMargin
      Parser.parse(stackLines(_))(in).get should be(
        Seq(
          Seq(None, Some('D'), None),
          Seq(Some('N'), Some('C'), None),
          Seq(Some('Z'), Some('M'), Some('P')),
        )
      )
    }


    "parse stack" in {
      val in =
        """    [D]    
          |[N] [C]    
          |[Z] [M] [P]
          | 1   2   3 
          |""".stripMargin

      Parser.parse(stack(_))(in).get should be(
        IndexedSeq(
          CargoStack('N', 'Z'),
          CargoStack('D', 'C', 'M'),
          CargoStack('P'),
        )
      )
    }

    "parse move" in {
      Parser.parse(move(_))("move 1 from 2 to 1\n").get should be(Move(1, 2, 1))
    }
    "parse moves" in {
      val in =
        """move 1 from 2 to 1
          |move 3 from 1 to 3
          |move 2 from 2 to 1
          |move 1 from 1 to 2
          |""".stripMargin

      Parser.parse(moves(_))(in).get should be(exampleMoves)
    }

    "parse" in {
      Day5.parse(exampleInput) should be(exampleModel)
    }

  }
  
  "State" should {
    "process step" in {
      exampleModel.step(strategy1) should contain (
        State(
          IndexedSeq(
            CargoStack('D', 'N', 'Z'),
            CargoStack('C', 'M'),
            CargoStack('P'),
          ),
          List(
            Move(3, 1, 3),
            Move(2, 2, 1),
            Move(1, 1, 2),
          ),
        )
      )

      exampleModel.solve(strategy1) should be(
        State(
          IndexedSeq(
            CargoStack('C'),
            CargoStack('M'),
            CargoStack('Z', 'N', 'D', 'P'),
          ),
          List(),
        )
      )
    }
  }
  
  "Day 5" should {
    "solve part 1" in {
      Day5.part1(exampleModel) should be("CMZ")
    }
    "solve part 2" in {
      Day5.part2(exampleModel) should be("MCD")
    }
  }

}
