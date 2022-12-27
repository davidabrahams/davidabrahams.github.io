package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class HitBonus(
    xCenter: Double,
    yCenter: Double,
    color: String,
    timeRemaining: Double
):
  def update(timeElapsed: Duration): HitBonus =
    this.copy(timeRemaining =
      Math.max(0, timeRemaining - timeElapsed.toUnit(SECONDS))
    )
  def draw(context: dom.CanvasRenderingContext2D): Unit =
    Util.fillText(context, "+1", xCenter, yCenter, 30, color)
