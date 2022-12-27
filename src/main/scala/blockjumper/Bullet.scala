package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

/** x, y store the top coordinates of the bullet
  */
case class Bullet(x: Double, y: Double):
  def update(timeElapsed: Duration): Bullet =
    this.copy(y = y + Bullet.Velocity * timeElapsed.toUnit(SECONDS))
  def isOffScreen = y > GameState.GrassHeight
  def hit(b: Block): Option[HitBonus] =
    val xHit = x > b.x && x < b.x + b.width
    val yHit = y < b.y + b.height && y > b.y - Bullet.Height
    if xHit && yHit then Some(HitBonus(x, b.y + b.height / 2, "#00FF7F", 0.8))
    else None

object Bullet:
  val Height = 120
  val Velocity = 2250 // original code had 45. 2250 = 45 * 50
  def drawBullets(
      bullets: List[Bullet],
      context: dom.CanvasRenderingContext2D
  ): Unit =
    context.beginPath()
    context.fillStyle = "#00FF7F"
    // the bullet's hit detection is a single verticle line. However we draw a
    // 3 pixel wide rectangle to make it more visible
    bullets.foreach(b => context.rect(b.x - 1, b.y, 3, Height))
    context.fill()
