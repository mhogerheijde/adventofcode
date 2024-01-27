package net.hogerheijde.aoc.util

sealed trait CircularBuffer[+T] {
  def headOption: Option[T]
  @deprecated("Use headOption")
  def currentOption: Option[T] = headOption
  def rotate: CircularBuffer[T]
  def rotate(i: Int): CircularBuffer[T]
}


object CircularBuffer {
  def apply: CircularBuffer[Nothing] = EmptyCircularBuffer
  def apply[T](items: T*): CircularBuffer[T] = {
    if (items.isEmpty) {
      EmptyCircularBuffer
    } else {
      new NonEmptyCircularBuffer[T](items.toIndexedSeq, 0)
    }
  }

  final class NonEmptyCircularBuffer[T] private[util](
      s: IndexedSeq[T],
      val pointer: Int = 0,
  ) extends CircularBuffer[T] {
    // for backwards compatibility.
    // Internal state should be an IndexedSeq,
    // but since the constructor was package private,
    // we'd break backwards compat if we change its type.
    private[util] val state: Seq[T] = s

    @deprecated
    private[util] def this(state: Seq[T]) = this(state.toIndexedSeq, 0)

    require(state.nonEmpty, "Empty state for non-empty circular buffer")
    require(pointer < state.size, "pointer is out of bounds")

    def size: Int = state.size
    val headOption: Option[T] = Some(state(pointer))
    val head: T = state(pointer)
    @deprecated("Use head")
    val current: T = head
    def apply(index: Int): T = state((pointer + index) % state.size)

    def rotate: NonEmptyCircularBuffer[T] = NonEmptyCircularBuffer(state.asInstanceOf[IndexedSeq[T]], (pointer + 1) % state.size)
    def rotate(i: Int): NonEmptyCircularBuffer[T] = i match {
      case _ if i == 0 => this
      case _ => NonEmptyCircularBuffer(state.asInstanceOf[IndexedSeq[T]], (pointer + (i % state.size) + state.size) % state.size)
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case cb: NonEmptyCircularBuffer[_] =>
          cb.size == this.size && (0 until this.size).forall (i => this(i) == cb(i))
        case _ => false
      }
    }

    override def hashCode(): Int = state.hashCode()
    override def toString: String = s"CircularBuffer(${state.mkString(", ")})"
  }

  object EmptyCircularBuffer extends CircularBuffer[Nothing] {
    override val headOption: Option[Nothing] = None
    override val rotate = this
    override def rotate(i: Int) = this

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

