import net.hogerheijde.aoc2016.days.day8.Command
import net.hogerheijde.aoc2016.days.day8.Rect

val rectPattern = "^rect ([0-9]+)x([0-9]+)".r
val columnPattern = "rotate row y=([0-9]+) by ([0-9]+)".r
val rowPattern = "rotate column x=([0-9]+) by ([0-9]+)".r
def parseCommand(input: String): String = {
  input match {
    case rectPattern(c, b) => s"$c, $b"
    case columnPattern(input) => "2"
    case rowPattern(input) => "3"
    case _ => "FOO"
  }
}

parseCommand("rect 14x2")
