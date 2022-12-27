package blockjumper

case class Rectangle(x: Double, y: Double, width: Double, height: Double):
  require(width > 0 && height > 0)
  def intersects(other: Rectangle): Boolean =
    val cond1 = other.x < x + width
    val cond2 = x < other.x + other.width
    val cond3 = other.y < y + height
    val cond4 = y < other.y + other.height
    cond1 && cond2 && cond3 && cond4
