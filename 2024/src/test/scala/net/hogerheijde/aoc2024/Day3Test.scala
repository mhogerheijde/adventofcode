package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.int
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2024.Day3.Garbage
import net.hogerheijde.aoc2024.Day3.Instruction
import net.hogerheijde.aoc2024.Day3.Multiply
import net.hogerheijde.aoc2024.Day3.regex
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Test extends AnyWordSpec with Matchers {

  val exampleInput: String =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"


  "Day 3 parser" should {

//    def instruction[$: P]: P[Instruction] = P(multiply | garbage)
//    def multiply[$: P]: P[Multiply] = P("mul(" ~ int ~ "," ~ int ~ ")" ~/ "").map((x, y) => Multiply(x, y))
//    def garbage[$: P]: P[Garbage] = P(AnyChar.rep.! ~ &(" ")).map(Garbage(_))

    "parse" in {
//      Parser.parse(multiply(_))("mul(2,4)%&mul[3,7]").get should be(Multiply(2, 4))
//      Parser.parse(garbage(_))("foobarbaz mul(2,4)%&mul[3,7]").get should be(Garbage("foobarbazmul(2,4)%&mul[3,7]"))

//      Day3.regex.

//      val reg = raw"^((.*?)(mul\([0-9]+,[0-9]+\)))+".r
//      exampleInput match
//        case reg(a*) => println(a)
//        case _ => println("Oh noes")

//
//      println(s"Result: $x")


//      exampleInput match {
//        case Day3.regex(a483*) => println(a483) //matched group(1) assigned to variable a483
//        case _ => // no match
//      }

//      val result =
//      def keyword[$: P]: P[String] = P(AnyChar.rep ~ "hello" ~ !" " ~ AnyChar.! ~ "world")
//      Parser.parse(keyword(_))("hello hello-world").get should be("-")

//      Parser.parse(instruction(_))("gobbeltygookmul(2,4)%&mul[3,7]").get should be(Garbage("gobbeltygook"))
//      Parser.parse(instruction(_))("gobbeltygookmul(2,4)%&mul[3,7]").get should be(Garbage("gobbeltygook"))


    }
  }

  "Day 3" should {

    "find multiply" in {
      Day3.findMultiply(exampleInput) should be(
        Seq(
          Multiply(2, 4),
          Multiply(5,5),
          Multiply(11,8),
          Multiply(8,5),
        )
      )
    }

//    "parse input" in {
//      Day3.parse(exampleInput) should be(
//        Seq(
//          Garbage("x"),
//          Multiply(2, 4),
//          Garbage("%&mul[3,7]!@^do_not_"),
//          Multiply(5,5),
//          Garbage("+mul(32,64]then("),
//          Multiply(11,8),
//          Multiply(8,5),
//          Garbage(")"),
//        )
//      )
//    }

    "Part1: example answer" in {
      Day3.part1(Day3.parse(exampleInput)) should be(161)
    }

    "Part2: example answer" in {
      Day3.part2(Day3.parse(exampleInput)) should be(0)
    }
  }
}