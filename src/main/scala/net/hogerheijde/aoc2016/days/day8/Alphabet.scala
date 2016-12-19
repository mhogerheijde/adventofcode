package net.hogerheijde.aoc2016.days.day8

object Alphabet {

  def resolve(input: String): String = {
    val lettersParts = (input.split("\n") map { line =>
      val x = (line.sliding(5, 5) map { _.toString })
      x.toArray
    }).transpose

    val letters = lettersParts map { letterParts => letterParts.mkString("\n") } flatMap { resolveLetter }

    letters.mkString
  }

  def resolveLetter(possibleLetter: String): Option[Char] = {
    letters find { case (char, pattern) =>
        pattern == possibleLetter
    } map { _._1 }
  }

  val E =
    """####.
      |#....
      |###..
      |#....
      |#....
      |####.""".stripMargin

  val F =
    """####.
      |#....
      |###..
      |#....
      |#....
      |#....""".stripMargin

  val Y =
    """#...#
      |#...#
      |.#.#.
      |..#..
      |..#..
      |..#..""".stripMargin

  val K =
    """#..#.
      |#.#..
      |##...
      |#.#..
      |#.#..
      |#..#.""".stripMargin

  val R =
    """###..
      |#..#.
      |#..#.
      |###..
      |#.#..
      |#..#.
      |""".stripMargin

  val I =
    """.###.
      |..#..
      |..#..
      |..#..
      |..#..
      |.###.""".stripMargin



  val J =
    """..##.
      |...#.
      |...#.
      |...#.
      |#..#.
      |.##..
    """.stripMargin


  private val letters = Map(
    'E' -> E,
    'F' -> F,
    'Y' -> Y,
    'K' -> K,
    'R' -> R,
    'I' -> I,
    'J' -> J
  )

}
