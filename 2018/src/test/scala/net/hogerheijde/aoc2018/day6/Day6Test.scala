package net.hogerheijde.aoc2018.day6

import org.scalatest.Matchers
import org.scalatest.WordSpec

class Day6Test extends WordSpec with Matchers {


  val exampleInput = Day6.parse(
    """1, 1
      |1, 6
      |8, 3
      |3, 4
      |5, 5
      |8, 9
    """.stripMargin)

  val exampleProcessed = exampleInput.withAreasResolved
  val gridWithRegion = exampleInput.toRegionGrid(32)

  "Grid" should {

    "have a nice toString" in {
      exampleInput.toString should be (
        """ . . . . . . . . . .
          | . A . . . . . . . .
          | . . . . . . . . . .
          | . . . . . . . . C .
          | . . . D . . . . . .
          | . . . . . E . . . .
          | . B . . . . . . . .
          | . . . . . . . . . .
          | . . . . . . . . . .
          | . . . . . . . . F .
          | . . . . . . . . . .""".stripMargin)
    }

    "processes example" in {
      exampleProcessed.toString should be(
        """ a a a a a . c c c c
          | a A a a a . c c c c
          | a a a d d e c c c c
          | a a d d d e c c C c
          | . . d D d e e c c c
          | b b . d e E e e c c
          | b B b . e e e e . .
          | b b b . e e e f f f
          | b b b . e e f f f f
          | b b b . f f f f F f
          | b b b . f f f f f f""".stripMargin)
    }

    "find left" in {
      exampleInput.left should be (1)
    }

    "find right" in {
      exampleInput.right should be (8)
    }

    "find top" in {
      exampleInput.top should be (1)
    }
    "find bottom" in {
      exampleInput.bottom should be (9)
    }

    "find area for" in {
      exampleProcessed.areaFor('D') should contain (9)
      exampleProcessed.areaFor('E') should contain (17)
    }

    "know when area is infinte" in {
      exampleProcessed.infinteAreas should be(Set('A', 'B', 'C', 'F'))
    }

    "find max area" in  {
      exampleProcessed.maxArea should be(17)
    }

    "find region" in {

      gridWithRegion.toString should be (
        """ . . . . . . . . . .
          | . A . . . . . . . .
          | . . . . . . . . . .
          | . . . # # # . . C .
          | . . # D # # # . . .
          | . . # # # E # . . .
          | . B . # # # . . . .
          | . . . . . . . . . .
          | . . . . . . . . . .
          | . . . . . . . . F .
          | . . . . . . . . . .""".stripMargin)
    }

    "find region size" in {
      gridWithRegion.regionSize should be (16)
    }

  }

}
