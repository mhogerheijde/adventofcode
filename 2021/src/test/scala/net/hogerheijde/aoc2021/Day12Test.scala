
package net.hogerheijde.aoc2021

import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc2021.Day12.Cave
import net.hogerheijde.aoc2021.Day12.CaveSystem
import net.hogerheijde.aoc2021.Day12.Path
import net.hogerheijde.aoc2021.Day12.Paths
import net.hogerheijde.aoc2021.Day12.allowedToVisitRule2
import net.hogerheijde.aoc2021.Day12.expandRoutes
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day12Test extends AnyWordSpec with Matchers {

  val exampleInput1 = Day12.parse(
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end
      |""".stripMargin
  )

  val exampleInput2 = Day12.parse(
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc
      |""".stripMargin
  )

  val exampleInput3 = Day12.parse(
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW
      |""".stripMargin
  )


  "Parser" should {
    "parse single cave specific" in {
      Parser.parse(Day12.startCave(_))("start") should be(Some(Cave.Start))
      Parser.parse(Day12.endCave(_))("end") should be(Some(Cave.End))
      Parser.parse(Day12.smallCave(_))("ab") should be(Some(Cave.Small("ab")))
      Parser.parse(Day12.smallCave(_))("c") should be(Some(Cave.Small("c")))
      Parser.parse(Day12.bigCave(_))("AB") should be(Some(Cave.Big("AB")))
      Parser.parse(Day12.bigCave(_))("C") should be(Some(Cave.Big("C")))
    }
    "parse single cave generic" in {
      Parser.parse(Day12.cave(_))("start") should be(Some(Cave.Start))
      Parser.parse(Day12.cave(_))("end") should be(Some(Cave.End))
      Parser.parse(Day12.cave(_))("ab") should be(Some(Cave.Small("ab")))
      Parser.parse(Day12.cave(_))("c") should be(Some(Cave.Small("c")))
      Parser.parse(Day12.cave(_))("AB") should be(Some(Cave.Big("AB")))
      Parser.parse(Day12.cave(_))("C") should be(Some(Cave.Big("C")))
    }
    "parse a single link" in {
      Parser.parse(Day12.link(_))("C-a\n") should be(Some(
        (Cave.Big("C"), Cave.Small("a"))
      ))
    }
    "parse a single link cave system" in {
      Parser.parse(Day12.system(_))("C-a\n") should be(Some(CaveSystem(
        Set(Cave.Big("C"), Cave.Small("a")),
        Map(
          Cave.Big("C") -> Set(Cave.Small("a")),
          Cave.Small("a") -> Set(Cave.Big("C")),
        )
      )))
    }

    "parse example" in {
      exampleInput1 should be (
        CaveSystem(
          Set(
            Cave.Start,
            Cave.End,
            Cave("A"),
            Cave("b"),
            Cave("c"),
            Cave("d"),
          ),
          Map(
            Cave.Start -> Set(Cave("A"), Cave("b")),
            Cave.End -> Set(Cave("A"), Cave("b")),
            Cave("A") -> Set(Cave.Start, Cave.End, Cave("b"), Cave("c")),
            Cave("b") -> Set(Cave.Start, Cave.End, Cave("A"), Cave("d")),
            Cave("c") -> Set(Cave("A")),
            Cave("d") -> Set(Cave("b")),
          )
        )
      )
    }
  }

  "System" should {
    "find path in very simple cave" in {
      val system = Day12.parse(
        """start-A
          |A-end
          |""".stripMargin
      )

      expandRoutes(system) should be (Paths(Set(
        Path("start,A,end")
      )))
    }

    "find path in simple cave with part 2 rules" in {
      val system = Day12.parse(
        """start-A
          |A-a
          |a-end
          |""".stripMargin
      )

      expandRoutes(system, allowedToVisitRule2) should be(Paths(Set(
        Path("start,A,a,end"),
        Path("start,A,a,A,a,end"),
      )))
    }


    "find paths in small example" in  {
      expandRoutes(exampleInput1) should be (Paths(Set(
        Path("start, A, b, A, c, A, end"),
        Path("start, A, b, A, end"),
        Path("start, A, b, end"),
        Path("start, A, c, A, b, A, end"),
        Path("start, A, c, A, b, end"),
        Path("start, A, c, A, end"),
        Path("start, A, end"),
        Path("start, b, A, c, A, end"),
        Path("start, b, A, end"),
        Path("start, b, end"),
      )))
    }

    "find paths in small example, part2 rules" in {
      expandRoutes(exampleInput1, allowedToVisitRule2) should be(Paths(Set(
        Path("start, A, b, A, b, A, c, A, end"),
        Path("start, A, b, A, b, A, end"),
        Path("start, A, b, A, b, end"),
        Path("start, A, b, A, c, A, b, A, end"),
        Path("start, A, b, A, c, A, b, end"),
        Path("start, A, b, A, c, A, c, A, end"),
        Path("start, A, b, A, c, A, end"),
        Path("start, A, b, A, end"),
        Path("start, A, b, d, b, A, c, A, end"),
        Path("start, A, b, d, b, A, end"),
        Path("start, A, b, d, b, end"),
        Path("start, A, b, end"),
        Path("start, A, c, A, b, A, b, A, end"),
        Path("start, A, c, A, b, A, b, end"),
        Path("start, A, c, A, b, A, c, A, end"),
        Path("start, A, c, A, b, A, end"),
        Path("start, A, c, A, b, d, b, A, end"),
        Path("start, A, c, A, b, d, b, end"),
        Path("start, A, c, A, b, end"),
        Path("start, A, c, A, c, A, b, A, end"),
        Path("start, A, c, A, c, A, b, end"),
        Path("start, A, c, A, c, A, end"),
        Path("start, A, c, A, end"),
        Path("start, A, end"),
        Path("start, b, A, b, A, c, A, end"),
        Path("start, b, A, b, A, end"),
        Path("start, b, A, b, end"),
        Path("start, b, A, c, A, b, A, end"),
        Path("start, b, A, c, A, b, end"),
        Path("start, b, A, c, A, c, A, end"),
        Path("start, b, A, c, A, end"),
        Path("start, b, A, end"),
        Path("start, b, d, b, A, c, A, end"),
        Path("start, b, d, b, A, end"),
        Path("start, b, d, b, end"),
        Path("start, b, end"),
      )))
    }

    "find paths in second example" in {
      expandRoutes(exampleInput2) should be (Paths(Set(
        Path("start,HN,dc,HN,end"),
        Path("start,HN,dc,HN,kj,HN,end"),
        Path("start,HN,dc,end"),
        Path("start,HN,dc,kj,HN,end"),
        Path("start,HN,end"),
        Path("start,HN,kj,HN,dc,HN,end"),
        Path("start,HN,kj,HN,dc,end"),
        Path("start,HN,kj,HN,end"),
        Path("start,HN,kj,dc,HN,end"),
        Path("start,HN,kj,dc,end"),
        Path("start,dc,HN,end"),
        Path("start,dc,HN,kj,HN,end"),
        Path("start,dc,end"),
        Path("start,dc,kj,HN,end"),
        Path("start,kj,HN,dc,HN,end"),
        Path("start,kj,HN,dc,end"),
        Path("start,kj,HN,end"),
        Path("start,kj,dc,HN,end"),
        Path("start,kj,dc,end"),
      )))
    }

    "find paths in larger example" in {
      println(exampleInput3.toDot)
      val value1 = expandRoutes(exampleInput3)

      println(value1)

      value1 should have (size(226))
    }

  }

  "Day 12" should {
    "part 1 large" in { Day12.part1(exampleInput3) should be (226) }
    "part 2 example 1" in { Day12.part2(exampleInput1) should be (36) }
    "part 2 example 2" in { Day12.part2(exampleInput2) should be (103) }
    "part 2 example 3" in { Day12.part2(exampleInput3) should be (3509) }
  }
}
