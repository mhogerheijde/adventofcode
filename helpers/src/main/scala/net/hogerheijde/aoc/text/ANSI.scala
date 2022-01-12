package net.hogerheijde.aoc.text

import scala.io.AnsiColor._

object AnsiHelpers {
  implicit class AnsiString(string: String) {
    def bold: String = s"$BOLD$string$RESET"
    def red: String = s"$RED$string$RESET"
    def green: String = s"$GREEN$string$RESET"
  }
}
