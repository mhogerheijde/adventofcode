import net.hogerheijde.aoc2016.days.day8.Grid
import net.hogerheijde.aoc2016.days.day8.Screen

println(new Screen(Grid(10, 2)).toString)

val screen = new Screen(Grid(7, 3))
  .rect(3, 2)
  .rotateColumn(1, 1)
  .rotateRow(0, 4)
  .rotateColumn(1, 1)



println(screen.toString)
println(screen.count)