package net.hogerheijde.aoc.text

import java.nio.charset.Charset

import scala.io.AnsiColor.*

object AnsiHelpers {
  private val dimmed = String(Array(27, 91, 50, 109).map(_.toByte), Charset.defaultCharset())

  implicit class AnsiString(string: String) {
    def bold: String = s"$BOLD$string$RESET"
    def red: String = s"$RED$string$RESET"
    def green: String = s"$GREEN$string$RESET"
    def dim: String = s"$dimmed$string$RESET"
  }
}
