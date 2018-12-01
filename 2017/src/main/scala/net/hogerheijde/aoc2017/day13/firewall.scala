package net.hogerheijde.aoc2017.day13



class Firewall private(inputLayers: Map[Int, Layer]) {
  private val layers = inputLayers


  def severity: Int = {
    layers.foldLeft(0) { case (severity, (depth, layer)) =>
      severity + (if (layer.detected(depth)) { depth * layer.range } else { 0 })
    }
  }

  def detect(i: Int): Boolean = {
    layer(i).detected(i)
  }
  def layer(depth: Int): Layer = layers.getOrElse(depth, InactiveLayer)

  override def equals(obj: Any): Boolean = obj match {
    case f: Firewall =>  this.layers == f.layers
    case _ => false
  }
  override def hashCode(): Int = this.layers.hashCode()

  override def toString: String = s"Firewall(${layers.toString})"
}
object Firewall {
  def apply(layers: Map[Int, Layer]): Firewall = new Firewall(layers)
}

sealed trait Layer {
  def range: Int
  def detected(picosecond: Int): Boolean
}

case object InactiveLayer extends Layer {
  val range = 0
  override def detected(picosecond: Int): Boolean = false
}

case class ActiveLayer(range: Int) extends Layer{
  private val modulo = (range - 1) * 2
  def detected(picosecond: Int): Boolean = {
    picosecond % modulo == 0
  }
}
