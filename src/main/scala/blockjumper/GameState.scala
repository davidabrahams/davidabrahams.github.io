package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class GameState(
    soldier: Soldier,
    points: Int,
    blocks: List[Block],
    powerUps: List[PowerUp],
    bullets: List[Bullet],
    hitBonuses: List[HitBonus],
    last10FrameDuration: List[Duration]
):

  def update(
      totalGameTimeSeconds: Double,
      timeElapsedSinceLastFrame: Duration,
      keyState: KeyState,
      rng: util.Random
  ): GameState =
    // shadow the soldier variable here to the version which has collected all the power ups
    val (soldier, collectedPowerUps) = this.soldier.collectPowerUps(powerUps)
    val newBlockSpawnOdds = Block.spawnRate(
      totalGameTimeSeconds
    ) * timeElapsedSinceLastFrame.toUnit(SECONDS)
    val maybeNewBlock: Option[Block] =
      if rng.nextDouble() < newBlockSpawnOdds
      then Some(Block.generateRandom(rng, soldier.getSpawnSide(keyState)))
      else None
    val maybeNewBullet: Option[Bullet] =
      if keyState.processXClick() then soldier.maybeSpawnBullet else None
    val doExplode = keyState.processZClick() && soldier.explosions > 0
    val allBlocksDestroyed =
      collectedPowerUps.contains(PowerUpInfo.DestroyAllBlocks)
    val shrinkBy = shrinkFactor(collectedPowerUps)
    val newHitBonuses: List[HitBonus] =
      blocks.flatMap { block =>
        bullets.flatMap(bullet => bullet.hit(block)) ++ soldier.explodedBlock(
          block
        )
      }

    GameState(
      soldier
        .copy(
          bullets =
            soldier.bullets - (if maybeNewBullet.isDefined then 1 else 0),
          explosions = soldier.explosions - (if doExplode then 1 else 0),
          explosionSecondsRemaining =
            if doExplode then 0.4 else soldier.explosionSecondsRemaining
        )
        .completeJumps
        .applyKeyPresses(keyState)
        .applyJumps
        .update(timeElapsedSinceLastFrame),
      points + collectedPowerUps.count(
        _ == PowerUpInfo.Points1
      ) + 5 * collectedPowerUps.count(
        _ == PowerUpInfo.Points5
      ) + newHitBonuses.length,
      maybeNewBlock.toList ++ blocks
        .filterNot(_ => allBlocksDestroyed)
        .filterNot(soldier.explodedBlock(_).isDefined)
        .filterNot(_.isOffScreen)
        .filterNot(block => bullets.exists(_.hit(block).isDefined))
        .map(_.shrink(shrinkBy))
        .map(_.update(timeElapsedSinceLastFrame)),
      PowerUp.spawnPowerUps(
        rng,
        timeElapsedSinceLastFrame,
        totalGameTimeSeconds
      ) ++ powerUps
        .filterNot(soldier.doesCollect)
        .filterNot(_.isOffScreen)
        .map(_.update(timeElapsedSinceLastFrame)),
      maybeNewBullet.toList ++ bullets
        .filterNot(_.isOffScreen)
        .map(_.update(timeElapsedSinceLastFrame)),
      newHitBonuses ++ hitBonuses
        .filterNot(_.timeRemaining == 0)
        .map(_.update(timeElapsedSinceLastFrame)),
      (timeElapsedSinceLastFrame +: last10FrameDuration).take(10)
    )

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    drawCounterWindow(context)
    Block.drawBlocks(blocks, context)
    powerUps.foreach(_.draw(context))
    drawFPS(
      context,
      (last10FrameDuration.length / last10FrameDuration
        .map(_.toUnit(SECONDS))
        .sum).toInt
    )
    hitBonuses.foreach(_.draw(context))
    Bullet.drawBullets(bullets, context)
    soldier.draw(context)
    drawCounters(context)

  def isOver: Boolean = blocks.exists(block => soldier.isHit(block))

  private def drawCounterWindow(context: dom.CanvasRenderingContext2D): Unit =
    val indicatorRadius = 15
    context.beginPath()
    context.fillStyle = "rgba(0, 0, 0, 0.15)"
    context.rect(
      GameState.ScreenWidth - 6 * indicatorRadius - 20,
      0,
      6 * indicatorRadius + 20,
      // 15 pixels to account for the empty space. 23 * 5 / 6 accounts for the height of the points text
      2 * indicatorRadius + 15 + 23 * 5 / 6
    )
    context.fill()

  private def drawCounters(context: dom.CanvasRenderingContext2D): Unit =
    val indicatorRadius = 15
    val indicatorFontSize = 15
    Util.fillText(
      context,
      s"${points} points",
      GameState.ScreenWidth - 3 * indicatorRadius - 10,
      // the center y is 5 pixels down, and then half of the font height. the font height is 23 * 5 / 6
      5 + 23 * 5 / 6 / 2,
      23,
      "#000000"
    )
    Util.drawCircleWithText(
      context,
      GameState.ScreenWidth - indicatorRadius - 5,
      // 10 pixels down plus the font height
      indicatorRadius + 10 + 23 * 5 / 6,
      indicatorRadius,
      PowerUpInfo.SuperJump.color,
      soldier.superJumps.toString,
      indicatorFontSize,
      PowerUpInfo.SuperJump.fontColor
    )
    Util.drawCircleWithText(
      context,
      GameState.ScreenWidth - 3 * indicatorRadius - 10,
      indicatorRadius + 10 + 23 * 5 / 6,
      indicatorRadius,
      PowerUpInfo.Bullets.color,
      soldier.bullets.toString,
      indicatorFontSize,
      PowerUpInfo.Bullets.fontColor
    )
    Util.drawCircleWithText(
      context,
      GameState.ScreenWidth - 5 * indicatorRadius - 15,
      indicatorRadius + 10 + 23 * 5 / 6,
      indicatorRadius,
      PowerUpInfo.Explosion.color,
      soldier.explosions.toString,
      indicatorFontSize,
      PowerUpInfo.Explosion.fontColor
    )

  private def drawFPS(context: dom.CanvasRenderingContext2D, fps: Int): Unit =
    context.fillStyle = "#000000"
    context.font = s"25px sans-serif";
    context.fillText(s"FPS: $fps", 5, 24)
    ()

  private def shrinkFactor(collectedPowerUps: List[PowerUpInfo]): Int =
    collectedPowerUps.foldLeft(1) {
      case (i, PowerUpInfo.ShrinkAllBlocks) => i * 2
      case (i, _)                           => i
    }

object GameState:
  val ScreenWidth = 800
  val ScreenHeight = 600
  val GrassHeight = 420
