import net.hogerheijde.aoc2016.days.day8.Alphabet
import net.hogerheijde.aoc2016.days.day8.Command
import net.hogerheijde.aoc2016.days.day8.Rect

val input =
  """####.####.####.#...##..#.####.###..####..###...##.
    |#....#....#....#...##.#..#....#..#.#......#.....#.
    |###..###..###...#.#.##...###..#..#.###....#.....#.
    |#....#....#......#..#.#..#....###..#......#.....#.
    |#....#....#......#..#.#..#....#.#..#......#..#..#.
    |####.#....####...#..#..#.#....#..#.#.....###..##..""".stripMargin

val x = Alphabet.resolve(input)


