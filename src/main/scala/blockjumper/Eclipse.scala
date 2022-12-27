package blockjumper

/** x, y store the top-left coordinate of the bounding rectangle
  */
case class Eclipse(x: Double, y: Double, width: Double, height: Double):
  def contains(otherX: Double, otherY: Double): Boolean =
    val rx = width / 2
    val ry = height / 2
    val tx = (otherX - (x + rx)) / rx
    val ty = (otherY - (y + ry)) / ry
    tx * tx + ty * ty < 1
