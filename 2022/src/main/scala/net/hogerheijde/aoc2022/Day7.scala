package net.hogerheijde.aoc2022

import java.lang.Integer.parseInt
import java.util.Comparator

import scala.annotation.targetName
import scala.util.Try

import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc2022.Day7.Filesystem
import net.hogerheijde.aoc2022.Day7.Path.ROOT

object Day7 extends Day[Int, Int] {
  override type Model = Filesystem

  override def parse(input: String): Model = input
    .linesIterator
    .foldLeft((Filesystem(Map()), ROOT)) { case ((fileSystem, currentPath), next) =>
      next match {
        case s"$$ cd $name" =>
          val newPath = currentPath + name
          (fileSystem.mkDir(newPath), newPath)
        case s"$$ ls" => (fileSystem, currentPath)
        case s"dir $name" =>
          val currentFolder = fileSystem(currentPath).asFolder
          val newFolder = currentFolder.copy(entries = currentFolder.entries :+ (currentPath + name))
          val newFilesystem = fileSystem.copy(items = fileSystem.items.updated(currentPath, newFolder))
          (newFilesystem, currentPath)
        case s"${IsInteger(size)} $name" =>
          val currentFolder = fileSystem(currentPath).asFolder
          val newFolder = currentFolder.copy(entries = currentFolder.entries :+ (currentPath + name))
          val newFilesystem = fileSystem.touch(currentPath + name, size)
          (newFilesystem, currentPath)
      }
    }._1


  override def part1(input: Model): Int = {
    input.items.values.collect {
      case folder: Folder => folder.size(input)
      case _ => 0
    }.filter(_ <= 100000).sum
  }

  override def part2(input: Model): Int = ???

  case class Path private(
      parts: Seq[String],
      absolute: Boolean,
  ) extends Comparable[Path] {
    require(absolute || parts.nonEmpty)

    lazy val parent: Path = this + ".."

    @targetName("resolve")
    def +(part: String): Path = part match {
      case "/" => ROOT
      case ".." => this match {
        case ROOT => ROOT
        case _ => copy(parts.init) // will fail if relative and going up to far
      }
      case name => copy(parts :+ name)
    }
    val name = if(parts.isEmpty && absolute) { "/" } else { parts.last }

    override def toString: String =
      (if(absolute) { "/" } else { "" }) + parts.mkString("/")

    override def compareTo(o: Path): Int = this.toString.compareTo(o.toString)
  }
  object Path {
    val ROOT = Path(Seq(), true)
    def apply(): Path = Path(Seq("."), false)
    def apply(s: String): Path = if (s.startsWith("/")) {
      Path(s.split("/").toSeq.drop(1), true)
    } else {
      Path(s.split("/").toSeq, false)
    }
    def apply(parts: String*): Path = if (parts.headOption.contains("/")) {
      Path(parts.toSeq.drop(1), true)
    } else {
      Path(parts.toSeq, false)
    }
  }

  case class Filesystem(
      items: Map[Path, Entry] = Map()
  ) {
    def apply(path: Path): Entry = items(path)

    def touch(path: Path, size: Int): Filesystem = {
      val parent = items(path.parent).asFolder
      val newItems: Map[Path, Entry] = (items + (path -> File(path, size)))
        .updated(path.parent, parent.add(path))
      copy(items = newItems)
    }

    def mkDir(path: Path): Filesystem = {
      if (!items.contains(path)) {
        copy(items.updated(path, Folder(path)))
      } else {
        this
      }
    }
    def ls(path: Path = ROOT, orderBy: Filesystem => Ordering[Path] = _ => NoChange): String =
      items(path).listing(filesystem = this, orderBy = orderBy(this))
  }



  sealed trait Entry {
    val path: Path
    def size(filesystem: Filesystem): Int

    def listing(
        indent: String = "",
        filesystem: Filesystem,
        orderBy: Ordering[Path],
    ): String
    def asFolder: Folder = this.asInstanceOf[Folder]
    def asFile: File = this.asInstanceOf[File]

  }
  case class File(path: Path, fileSize: Int) extends Entry {
    def size(filesystem: Filesystem) = fileSize
    override def listing(
        indent: String = "",
        filesystem: Filesystem,
        orderBy: Ordering[Path],
    ): String = s"$indent- ${path.name} (file, size=$fileSize)\n"
  }
  object File {
    def apply(path: String, fileSize: Int): File = File(Path(path), fileSize)
  }
  case class Folder(path: Path, entries: Seq[Path] = Seq()) extends Entry {
    def size(filesystem: Filesystem): Int = entries.map(p => filesystem(p).size(filesystem)).sum
    def add(file: Path): Folder = copy(entries = entries :+ file)

    def listing(
        indent: String = "",
        filesystem: Filesystem,
        orderBy: Ordering[Path] = NoChange,
    ): String = {
      s"$indent- ${path.name} (dir)\n" +
        entries.sorted(orderBy)
          .map { filesystem(_) }
          .map {
            case folder: Folder => folder.listing(indent + "  ", filesystem, orderBy)
            case file: File => file.listing(indent + "  ", filesystem, orderBy)
          }.mkString("")
    }
  }
  object Folder {
    def apply(path: String, entries: Seq[String]): Folder = Folder(Path(path), entries.map(Path(_)))
  }

  object NoChange extends Ordering[Path]:
    override def compare(x: Path, y: Path): Int = 0

  case class ByName(dirsFirst: Boolean)(filesystem: Filesystem) extends Ordering[Path] {
    def byDirByName(p1: Path, p2: Path): Int = (filesystem(p1), filesystem(p2)) match {
      case (_: File, _: Folder) => 1
      case (_: Folder, _: File) => -1
      case (entry1, entry2) => entry1.path.compareTo(entry2.path)
    }

    def byName(p1: Path, p2: Path): Int = filesystem(p1).path.compareTo(filesystem(p2).path)

    override def compare(p1: Path, p2: Path): Int = if(dirsFirst) byDirByName(p1, p2) else { byName(p1, p2) }
  }
  object Sort {
    def byName(dirsFirst: Boolean): Filesystem => Ordering[Path] = { fs => ByName(dirsFirst)(fs) }
  }
}
