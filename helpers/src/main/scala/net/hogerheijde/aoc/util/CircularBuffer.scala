package net.hogerheijde.aoc.util

sealed trait CircularBuffer[+T] {
  def currentOption: Option[T]
  def rotate: CircularBuffer[T]
}


object CircularBuffer {
  def apply: CircularBuffer[Nothing] = EmptyCircularBuffer
  def apply[T](items: T*): CircularBuffer[T] = {
    if (items.isEmpty) {
      EmptyCircularBuffer
    } else {
      new NonEmptyCircularBuffer[T](items)
    }
  }

  final class NonEmptyCircularBuffer[T] private[util](val state: Seq[T]) extends CircularBuffer[T] {
    val currentOption: Option[T] = state.headOption
    val current: T = state.head

    def rotate: NonEmptyCircularBuffer[T] = {
      state match {
        case Seq() => this
        case Seq(_) => this
        case head +: tail =>
          new NonEmptyCircularBuffer(tail :+ head)
      }
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case necb: NonEmptyCircularBuffer[_] => necb.state == this.state
        case _ => false
      }
    }

    override def hashCode(): Int = state.hashCode()
    override def toString: String = s"CircularBuffer(${state.mkString(", ")})"
  }

  object EmptyCircularBuffer extends CircularBuffer[Nothing] {
    val currentOption: Option[Nothing] = None
    val rotate = this

    override def equals(other: Any): Boolean = {
      other match {
        case EmptyCircularBuffer => true
        case _ => false
      }
    }

    override def hashCode(): Int = super.hashCode()

    override def toString: String = "CircularBuffer()"
  }

}

