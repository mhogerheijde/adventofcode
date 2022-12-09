package net.hogerheijde.aoc2022

import net.hogerheijde.aoc2022.Day7.ByName
import net.hogerheijde.aoc2022.Day7.File
import net.hogerheijde.aoc2022.Day7.Filesystem
import net.hogerheijde.aoc2022.Day7.Folder
import net.hogerheijde.aoc2022.Day7.Path
import net.hogerheijde.aoc2022.Day7.Path.ROOT
import net.hogerheijde.aoc2022.Day7.Sort
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day7Test extends AnyWordSpec with Matchers {

  val exampleInput =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k
      |""".stripMargin

  val exampleFilesystem = Filesystem(
    Map(
      Path("/") -> Folder("/", Seq("/a", "/b.txt", "/c.dat", "/d")),
      Path("/a") -> Folder("/a", Seq("/a/e", "/a/f", "/a/g", "/a/h.lst")),
      Path("/a/e") -> Folder("/a/e", Seq("/a/e/i")),
      Path("/a/e/i") -> File("/a/e/i", 584),
      Path("/a/f") -> File("/a/f", 29116),
      Path("/a/g") -> File("/a/g", 2557),
      Path("/a/h.lst") -> File("/a/h.lst", 62596),
      Path("/b.txt") -> File("/b.txt", 14848514),
      Path("/c.dat") -> File("/c.dat", 8504156),
      Path("/d") -> Folder("/d", Seq("/d/j", "/d/d.log", "/d/d.ext", "/d/k")),
      Path("/d/d.ext") -> File("/d/d.ext", 5626152),
      Path("/d/j") -> File("/d/j", 4060174),
      Path("/d/d.log") -> File("/d/d.log", 8033020),
      Path("/d/k") -> File("/d/k", 7214296),
    )
  )

  val exampleListing =
    """- / (dir)
      |  - a (dir)
      |    - e (dir)
      |      - i (file, size=584)
      |    - f (file, size=29116)
      |    - g (file, size=2557)
      |    - h.lst (file, size=62596)
      |  - b.txt (file, size=14848514)
      |  - c.dat (file, size=8504156)
      |  - d (dir)
      |    - j (file, size=4060174)
      |    - d.log (file, size=8033020)
      |    - d.ext (file, size=5626152)
      |    - k (file, size=7214296)
      |""".stripMargin

  "Day 7" should {
    "parse example into Filesystem" in {
      Day7.parse(exampleInput) should be(exampleFilesystem)
    }
    "solve part 1" in  {
      Day7.part1(exampleFilesystem) should be (95437)
    }
    "solve part 2" in  {
      Day7.part2(exampleFilesystem) should be (24933642)
    }

  }

  val fs = Filesystem(
    Map(
      ROOT -> Folder(ROOT, Seq(
        Path("/hello.txt"),
        Path("/foo"),
        Path("/bar"),
        Path("/qux.txt"),
        Path("/zzz"),
        Path("/bbb"),
      )),
      Path("/hello.txt") -> File(Path("/hello.txt"), 123),
      Path("/foo") -> File(Path("/foo"), 222),
      Path("/bar") -> File(Path("/bar"), 555),
      Path("/qux.txt") -> File(Path("/qux.txt"), 645),
      Path("/zzz") -> Folder(Path("/zzz"), Seq()),
      Path("/bbb") -> Folder(Path("/bbb"), Seq()),
    )
  )

  "Filesystem" should {
    "list" in {
      exampleFilesystem.ls() should be(exampleListing)
    }
    "calc size" in {
      exampleFilesystem(Path("/a/e")).size(exampleFilesystem) should be (584)
      exampleFilesystem(Path("/a")).size(exampleFilesystem) should be (94853)
      exampleFilesystem(Path("/d")).size(exampleFilesystem) should be (24933642)
      exampleFilesystem(Path("/")).size(exampleFilesystem) should be (48381165)
    }

    "calc free" in {
      exampleFilesystem.free should be(21618835)
    }


  }

  "Folder" should {

    "have size" in {
      fs(ROOT).size(fs) should be (123 + 222 + 555 + 645)
    }

    "print empty folder" in {
      Folder(ROOT, Seq()).listing("", Filesystem()) should be("- / (dir)\n")
    }
    "print files" in {
      Folder(ROOT, Seq(
        Path("/hello.txt"),
        Path("/foo"),
        Path("/bar"),
        Path("/qux.txt"),
      ))
        .listing("", fs) should be(
          """- / (dir)
            |  - hello.txt (file, size=123)
            |  - foo (file, size=222)
            |  - bar (file, size=555)
            |  - qux.txt (file, size=645)
            |""".stripMargin
        )
    }

    "print files indented" in {
      Folder(
        ROOT, Seq(
          Path("/hello.txt"),
          Path("/foo"),
          Path("/bar"),
          Path("/qux.txt"),
        ))
        .listing("  ", fs) should be(
        """  - / (dir)
          |    - hello.txt (file, size=123)
          |    - foo (file, size=222)
          |    - bar (file, size=555)
          |    - qux.txt (file, size=645)
          |""".stripMargin
      )
    }

    "print files sorted" in {
      Folder(
        ROOT, Seq(
          Path("/hello.txt"),
          Path("/foo"),
          Path("/bar"),
          Path("/qux.txt"),
        ))
        .listing("", fs, ByName(true)(fs)) should be(
        """- / (dir)
          |  - bar (file, size=555)
          |  - foo (file, size=222)
          |  - hello.txt (file, size=123)
          |  - qux.txt (file, size=645)
          |""".stripMargin
      )
    }

    "print folders with files" in {
      Folder(
        ROOT, Seq(
          Path("/hello.txt"),
          Path("/zzz"),
          Path("/foo"),
          Path("/bar"),
          Path("/bbb"),
          Path("/qux.txt"),
        ))
        .listing("", fs, ByName(true)(fs)) should be(
        """- / (dir)
          |  - bbb (dir)
          |  - zzz (dir)
          |  - bar (file, size=555)
          |  - foo (file, size=222)
          |  - hello.txt (file, size=123)
          |  - qux.txt (file, size=645)
          |""".stripMargin
      )
    }

    "print folders with content" in {

    }
  }


  "Path" should {
    "create from string" in {
      Path("/") should be(ROOT)
      Path("/foo") should be(Path("/", "foo"))
      Path("/foo/bar") should be(Path("/", "foo", "bar"))
      Path("foo/bar") should be(Path("foo", "bar"))
    }

    "toString for absolute paths" in {
      Path("/").toString should be ("/")
      Path("/", "hello").toString should be ("/hello")
      Path("/", "hello", "world").toString should be ("/hello/world")
    }

    "toString for relative paths" in {
      Path("..").toString should be("..")
      Path("..", "hello").toString should be("../hello")
      Path("foo", "bar", "baz").toString should be("foo/bar/baz")
    }

    "resolve" in {
      Path("/", "hello", "world") + ".." should be(Path("/", "hello"))

      val expect = Path("/")
      val result = Path("/", "hello") + ".."
      result should be(expect)
      Path("/") + ".." should be(Path("/"))

      Path("/", "hello", "world") + "/" should be(Path("/"))

      Path("/", "hello", "world") + "bar" should be(Path("/", "hello", "world", "bar"))
      Path("/", "hello") + "bar" should be(Path("/", "hello", "bar"))
      Path("/") + "bar" should be(Path("/", "bar"))
    }
  }

}
