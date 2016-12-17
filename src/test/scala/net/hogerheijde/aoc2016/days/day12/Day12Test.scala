package net.hogerheijde.aoc2016.days.day12

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class Day12Test extends FlatSpec with Matchers {

  "Day12" should "parse a inc instruction" in {
    Day12.parseLine("inc a") should be (Inc(A))
    Day12.parseLine("inc b") should be (Inc(B))
    Day12.parseLine("inc c") should be (Inc(C))
    Day12.parseLine("inc d") should be (Inc(D))
  }

  it should "parse a dec instruction" in {
    Day12.parseLine("dec a") should be (Dec(A))
    Day12.parseLine("dec b") should be (Dec(B))
    Day12.parseLine("dec c") should be (Dec(C))
    Day12.parseLine("dec d") should be (Dec(D))
  }


  for {
    source <- IndexedSeq(A, B, C, D)
    target <- IndexedSeq(A, B, C, D)
    line = s"cpy $source $target"
  } yield {
    it should s"parse instruction `cpy $source $target`" in {
      Day12.parseLine(line) should be(Copy(Right(source), target))
    }
  }


  for {
    source <- Range.inclusive(-10, 10) // negative double digits to positive double digits in order to check not only 1 character pos. is parsed
    target <- IndexedSeq(A, B, C, D)
    line = s"cpy $source $target"
  } yield {
    it should s"parse instruction `cpy $source $target`" in {
      Day12.parseLine(line) should be(Copy(Left(source), target))
    }
  }

  for {
    register <- IndexedSeq(A, B, C, D)
    jump <- Range.inclusive(-10, 10) // negative double digits to positive double digits in order to check not only 1 character pos. is parsed
    line = s"jnz $register $jump"
  } yield {
    it should s"parse instruction `jnz $register $jump`" in {
      Day12.parseLine(line) should be(Jnz(register, jump))
    }
  }


}
