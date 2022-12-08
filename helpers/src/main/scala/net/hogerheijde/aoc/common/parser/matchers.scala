package net.hogerheijde.aoc.common.parser

import scala.util.Try

object IsInteger {
  def unapply(s: String): Option[Int] = Try { Integer.parseInt(s, 10) }.toOption
}
