package net.hogerheijde.aoc2024

import fastparse.*
import fastparse.NoWhitespace.*
import net.hogerheijde.aoc.common.parser.Common.digit
import net.hogerheijde.aoc.common.parser.IsInteger
import net.hogerheijde.aoc.text.AnsiHelpers
import net.hogerheijde.aoc.util.Day
import net.hogerheijde.aoc.util.Parser
import net.hogerheijde.aoc.text.AnsiHelpers.AnsiString
import net.hogerheijde.aoc2024.Day9.Block.FileParts
import net.hogerheijde.aoc2024.Day9.Block.Space

import scala.collection.immutable
import scala.util.Try

object Day9 extends Day[Long, Long]:

  type Model = Disk
  type IsCompacted = Disk => Boolean

  override def parse(input: String): Model = Parser.parse(disk(_))(input).get

  override def part1(input: Model): Long = input
    .defrag(defragStrategy1, isCompacted1)
    .checksum()

  override def part2(input: Model): Long = input
    .defrag(defragStrategy2, isCompacted2)
    .checksum()

  case class Disk(map: IndexedSeq[Block] = IndexedSeq()):
    def checksum(): Long =
      map
        .flatMap(b => b.explode)
        .zipWithIndex
        .filter(_._1 != -1)
        .map((id, idx) => id.toLong * idx.toLong)
        .sum

    def defrag1Step(): Disk = defragStrategy1(this, isCompacted1)
    def defrag2Step(): Disk = defragStrategy2(this, isCompacted2)

    def defrag(f: (Disk, IsCompacted) => Disk, isCompacted: IsCompacted): Disk =
      val x: Iterator[Disk] = Iterator.unfold(this) { disk =>
        val newdisk = f(disk, isCompacted)
        Some(newdisk, newdisk)
      }
      x.find(isCompacted(_)).get
    override def toString: String = map.mkString

  def isCompacted1(disk: Disk): Boolean = !disk.map.exists {
    case _: Space => true
    case _ => false
  }
  def defragStrategy1(disk: Disk, isCompacted: IsCompacted): Disk =
    if isCompacted(disk) then
      disk
    else
      val lastBlock = disk.map.last.asInstanceOf[FileParts]

      val blocks = disk.map.init.foldLeft((IndexedSeq[Block](), Option(lastBlock))) {
        case ((acc, None), nextBlock) =>
          (acc :+ nextBlock, None) // just append the rest
        case ((acc, Some(lastBlock)), space: Space) =>
          val (x, y, z) = space.useBy(lastBlock)
          (acc ++ Seq(Some(x), y).flatten, z)
        case ((acc, lastBlock), nextBlock: FileParts) =>
          (acc :+ nextBlock, lastBlock)
      }
      val withoutTrailingSpace = blocks._1 match
        case init :+ (_: Space) => init
        case allBlocks => allBlocks

      Disk(withoutTrailingSpace ++ blocks._2.toSeq)


  def isCompacted2(disk: Disk): Boolean = disk.map.forall {
    case _: Space => true
    case b: FileParts => !b.moveable
  }
  def defragStrategy2(disk: Disk, isCompacted: Disk => Boolean): Disk =
    if isCompacted(disk) then
      disk
    else
      val lastMovablePart = disk.map.lastIndexWhere {
        case b: FileParts => b.moveable
        case _ => false
      }
      val firstSpace = disk.map.indexWhere {
        case s: Space if s.size >= disk.map(lastMovablePart).size => true
        case _ => false
      }
      if firstSpace < 0 || firstSpace > lastMovablePart then
        Disk(disk.map.updated(lastMovablePart, disk.map(lastMovablePart).asInstanceOf[FileParts].copy(moveable = false)))
      else {
        val partToMove = disk.map(lastMovablePart).asInstanceOf[FileParts].copy(moveable = false)
        val replace: IndexedSeq[Block] = disk.map(firstSpace).asInstanceOf[Space].useBy(partToMove) match {
          case (file, Some(spaceLeft), None) => IndexedSeq(file, spaceLeft)
          case (file, None, None) => IndexedSeq(file)
          case _ => throw new Exception("We should not get here")
        }

        Disk(
          (disk.map.take(firstSpace)
            ++ replace
            ++ disk.map.slice(firstSpace + 1, lastMovablePart)
            :+ partToMove.asSpace)
            ++ disk.map.drop(lastMovablePart + 1)
        )
      }

  opaque type Identifier = Int
  object Identifier:
    def apply(i: Int): Identifier = i

  sealed trait Block:
    val id: Identifier
    val size: Int
    val symbol: String
    def explode: Seq[Identifier] = Range(0, size).map(_ => id).toSeq
    override def toString: String = symbol * size

  object Block:
    case class Space(size: Int) extends Block:
      override val id: Identifier = -1
      override val symbol: String = "."
      def useBy(file: FileParts): (FileParts, Option[Space], Option[FileParts]) =
        file.size - size match
          case 0 => (file, None, None)
          case fileSizeLeft if fileSizeLeft > 0 =>
            (FileParts(file.id, size, moveable = false), None, Some(FileParts(file.id, fileSizeLeft, moveable = true)))
          case spaceLeft if spaceLeft < 0 =>
            (file, Some(Space(math.abs(spaceLeft))), None)

    case class FileParts(id: Identifier, size: Int, moveable: Boolean = true) extends Block:
      override val symbol: String = id.toString
      def asSpace: Space = Space(size)

      override def toString: String = {
        if moveable then
          super.toString
        else
          s"[${super.toString}]"
      }

  def sequence[$: P]: P[Seq[Block]] = P(Index ~ CharIn("0-9").! ~ CharIn("0-9").!)
    .map { case (i, f, s) => Seq(FileParts(Identifier(i / 2), f.toInt), Space(s.toInt)) }
  def disk[$: P]: P[Disk] = P(sequence.rep(min = 1) ~ (Index ~ digit).? ~ "\n".? ~ End).map { case (x, file) =>
    Disk(x.flatten.toIndexedSeq ++ file.map((i, f) => FileParts(i / 2, f)).toSeq)
  }