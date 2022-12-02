package net.hogerheijde.aoc2022


import net.hogerheijde.aoc.util.Day

object Day2 extends Day[Int, Int]{
  override type Model = Seq[(Play, Response)]

  sealed trait Play
  object Play {
    case object Rock extends Play
    case object Paper extends Play
    case object Scissors extends Play

    def parse(in: String): Play = in match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
      case x => throw new RuntimeException(s"Cannot parse $x as challenge")
    }
  }

  sealed trait Response
  object Response {
    case object X extends Response
    case object Y extends Response
    case object Z extends Response

    def parse(in: String): Response = in match {
      case "X" => X
      case "Y" => Y
      case "Z" => Z
      case x => throw new RuntimeException(s"Cannot parse $x as response")
    }
  }

  override def parse(input: String): Model = input
    .linesIterator
    .map { line => line.split(" ", 2) }
    .map { parts => (parts(0), parts(1)) }
    .map { case (challenge, response) =>
      (Play.parse(challenge), Response.parse(response))
    }
    .toSeq

  val strategy1: (Play, Response) => Play = {
    case (_, Response.X) => Play.Rock
    case (_, Response.Y) => Play.Paper
    case (_, Response.Z) => Play.Scissors
  }
  val strategy2: (Play, Response) => Play = {
    case (Play.Rock, Response.X) => Play.Scissors
    case (Play.Rock, Response.Y) => Play.Rock
    case (Play.Rock, Response.Z) => Play.Paper

    case (Play.Paper, Response.X) => Play.Rock
    case (Play.Paper, Response.Y) => Play.Paper
    case (Play.Paper, Response.Z) => Play.Scissors

    case (Play.Scissors, Response.X) => Play.Paper
    case (Play.Scissors, Response.Y) => Play.Scissors
    case (Play.Scissors, Response.Z) => Play.Rock
  }

  def score(challenge: Play, response: Response, strategy: (Play, Response) => Play) = calc(challenge, strategy(challenge, response))

  def calc(challenge: Play, response: Play): Int = (challenge, response) match {
    case (Play.Rock, Play.Rock) => 3 + 1
    case (Play.Rock, Play.Paper) => 6 + 2
    case (Play.Rock, Play.Scissors) => 0 + 3

    case (Play.Paper, Play.Rock) => 0 + 1
    case (Play.Paper, Play.Paper) => 3 + 2
    case (Play.Paper, Play.Scissors) => 6 + 3

    case (Play.Scissors, Play.Rock) => 6 + 1
    case (Play.Scissors, Play.Paper) => 0 + 2
    case (Play.Scissors, Play.Scissors) => 3 + 3
  }

  override def part1(input: Model): Int = input.map { case (c, r) => score(c, r, strategy1) }.sum

  override def part2(input: Model): Int = input.map { case (c, r) => score(c, r, strategy2) }.sum
}
