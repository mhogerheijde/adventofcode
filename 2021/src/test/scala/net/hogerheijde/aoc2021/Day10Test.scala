
package net.hogerheijde.aoc2021

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Test extends AnyWordSpec with Matchers {

  val exampleInput = {
    val input =
      """[({(<(())[]>[[{[]{<()<>>
        |[(()[<>])]({[<{<<[]>>(
        |{([(<{}[<>[]}>{[]{[(<()>
        |(((({<>}<{<{<>}{[]{[]{}
        |[[<[([]))<([[{}[[()]]]
        |[{[{({}]{}}([{[{{{}}([]
        |{<[[]]>}<{[{[{[]{()[[[]
        |[<(<(<(<{}))><([]([]()
        |<{([([[(<>()){}]>(<<{{
        |<{([{{}}[<[[[<>{}]]]>[]]
        |""".stripMargin
    Day10.parse(input)
  }

  "Day 9" should {
    "part 1" in { Day10.part1(exampleInput) should be (26397) }
    "part 2" in { Day10.part2(exampleInput) should be (288957) }
  }
}
