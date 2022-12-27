package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

/** @param firstAppear
  *   when the power up first appears, in seconds
  * @param rate
  *   how often it spawns, in count/second
  */
enum PowerUpInfo(
    val text: String,
    val color: String,
    val firstAppear: Double,
    val rate: Double,
    val fontSize: Int,
    val fontColor: String
):
  case Points1 extends PowerUpInfo("+1 Point", "#F8D0C6", 5, .3, 13, "#000000")
  // 7.5, .1333 is correct
  case SuperJump
      extends PowerUpInfo("Super\nJump", "#E84023", 15, .10, 14, "#FFFFFF")
  // 10, .0833 is correct
  case Bullets
      extends PowerUpInfo("+5\nBullets", "#8DA057", 25, .10, 14, "#000000")
  case Points5
      extends PowerUpInfo("+5 Point", "#E19999", 40, .04, 13, "#000000")
  // 25, .05 is correct
  case ShrinkAllBlocks
      extends PowerUpInfo(
        "Shrink\nAll\nBlocks",
        "#000000",
        45,
        .025,
        11,
        "#FFFFFF"
      )
  // 15, .0667 is correct
  case Invincibility
      extends PowerUpInfo("Invincibility", "#D2FBCE", 60, .075, 10, "#000000")
  // 20, .0833 is correct
  case Explosion
      extends PowerUpInfo("Explosion", "#EC8330", 80, .1, 11, "#000000")
  // 27.5, .05 is correct
  case DestroyAllBlocks
      extends PowerUpInfo(
        "Destroy\nAll\nBlocks",
        "#FFFFFF",
        100,
        .08,
        11,
        "#000000"
      )
  // 30, .0333 is correct
  case UltimateInvincibility
      extends PowerUpInfo(
        "Ultimate\nInvincibility",
        "#CC7AF5",
        120,
        .035,
        10,
        "#000000"
      )

case class PowerUp(centerX: Double, centerY: Double, info: PowerUpInfo):
  def update(timeElapsed: Duration): PowerUp =
    this.copy(centerY =
      centerY + PowerUp.Velocity * timeElapsed.toUnit(SECONDS)
    )
  def isOffScreen = centerY - PowerUp.Radius > GameState.GrassHeight
  def draw(context: dom.CanvasRenderingContext2D): Unit =
    Util.drawCircleWithText(
      context,
      centerX,
      centerY,
      PowerUp.Radius,
      info.color,
      info.text,
      info.fontSize,
      info.fontColor
    )

  def eclipse = Eclipse(
    centerX - PowerUp.Radius,
    centerY - PowerUp.Radius,
    PowerUp.Radius * 2,
    PowerUp.Radius * 2
  )

object PowerUp:
  val Radius = 25
  val Velocity = 200 // original code had 4

  def spawnPowerUps(
      rng: util.Random,
      timeElapsed: Duration,
      totalGameTimeSeconds: Double
  ): List[PowerUp] =
    PowerUpInfo.values.collect {
      case powerUpInfo
          if totalGameTimeSeconds > powerUpInfo.firstAppear && rng
            .nextDouble() < powerUpInfo.rate * timeElapsed.toUnit(SECONDS) =>
        PowerUp(
          // valid centerX range is [radius, width - radius]
          rng.nextDouble * (GameState.ScreenWidth - 2 * Radius) + Radius,
          // start the power up completely off screen
          -Radius,
          powerUpInfo
        )
    }.toList
