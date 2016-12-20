import net.hogerheijde.aoc2016.days.day11.Elevator
import net.hogerheijde.aoc2016.days.day11.F1
import net.hogerheijde.aoc2016.days.day11.F4
import net.hogerheijde.aoc2016.days.day11.Generator
import net.hogerheijde.aoc2016.days.day11.Microchip
import net.hogerheijde.aoc2016.days.day11.State
import net.hogerheijde.aoc2016.days.day11.ValidState

println(
  ValidState(Elevator(F4), items = Set(
    Microchip("FM", F4),
    Generator("FG", F4),
    Microchip("QM", F1),
    Generator("QG", F1)
  )).toString
)


val items = IndexedSeq('1', '2', '3', '4', '5')
val x = (1 to 2).flatMap(items.combinations)
x map {
  println
}
