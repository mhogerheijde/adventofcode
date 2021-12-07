package net.hogerheijde.aoc2020

import net.hogerheijde.aoc.util.Parser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day7Test extends AnyWordSpec with Matchers {

  val exampleRules =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  val exampleRules2 =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.
      |""".stripMargin


  "Day 7" should {
    "parse example" in {

      Parser.parse(Day7.label(_))("light red").get should be ("light red")
      Parser.parse(Day7.bag(_))("light red bag").get should be ("light red")
      Parser.parse(Day7.bag(_))("light red bags").get should be ("light red")
      Parser.parse(Day7.countedBags(_))("2 light red bags").get should be ((2, "light red"))
      Parser.parse(Day7.rule(_))("light red bags contain 1 bright white bag, 2 muted yellow bags.").get should be (
        ("light red", Map("bright white" -> 1, "muted yellow" -> 2))
      )

      Day7.parse(exampleRules) should be (Map(
        "light red" -> Map("bright white" -> 1, "muted yellow" -> 2),
        "dark orange" -> Map("bright white" -> 3, "muted yellow" -> 4),
        "bright white" -> Map("shiny gold" -> 1),
        "muted yellow" -> Map("shiny gold" -> 2, "faded blue" -> 9),
        "shiny gold" -> Map("dark olive" -> 1, "vibrant plum" -> 2),
        "dark olive" -> Map("faded blue" -> 3, "dotted black" -> 4),
        "vibrant plum" -> Map("faded blue" -> 5, "dotted black" -> 6),
        "faded blue" -> Map(),
        "dotted black" -> Map(),
      ))
    }

    "to dot file" in {
      val rules = Day7.parse(exampleRules)
      Day7.toDot(rules) should be ("")
    }



    "can contain gold" in {
      val canContainGold = Day7.canContain("shiny gold", _, _)

      val rules = Day7.parse(exampleRules)

      canContainGold("shiny gold", rules) should be (false)
      canContainGold("dark olive", rules) should be (false)
      canContainGold("vibrant plum", rules) should be (false)
      canContainGold("faded blue", rules) should be (false)
      canContainGold("dotted black", rules) should be (false)


      canContainGold("bright white", rules) should be (true)
      canContainGold("muted yellow", rules) should be (true)
      canContainGold("dark orange", rules) should be (true)
      canContainGold("light red", rules) should be (true)

    }

    "should count part 1" in {
      val rules = Day7.parse(exampleRules)

      Day7.part1(rules) should be (4)
    }

    "should count part 2" in {
      val rules = Day7.parse(exampleRules)

      Day7.countContainingBags("faded blue", rules) should be (0)
      Day7.countContainingBags("dotted black", rules) should be (0)
      Day7.countContainingBags("vibrant plum", rules) should be (11)
      Day7.countContainingBags("dark olive", rules) should be (7)
      Day7.countContainingBags("shiny gold", rules) should be (1 + 1*7 + 2 + 2*11)


      val rules2 = Day7.parse(exampleRules2)
      Day7.countContainingBags("shiny gold", rules2) should be (126)
    }

  }

}
