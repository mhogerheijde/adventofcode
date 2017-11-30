import net.hogerheijde.aoc2016.days.day9.Day9

val input = "ABC(1x1)DEF".iterator

input.isEmpty

val unexpanded = input.takeWhile(_ != '(').length

val (size, times) = Day9.processExpansion(input)
val newIterator = input.drop(size)


val unexpanded2 = newIterator.takeWhile(_ != '(').length
val (size2, times2) = Day9.processExpansion(newIterator)



