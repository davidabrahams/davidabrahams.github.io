package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class Soldier(
    x: Double,
    y: Double,
    xVelocity: Double,
    yVelocity: Double,
    superJumps: Int,
    midSuperJump: Boolean,
    regularJumpQueued: Boolean,
    superJumpQueued: Boolean,
    invincibilitySecondsRemaining: Double,
    ultimateInvincibilitySecondsRemaining: Double,
    explosionSecondsRemaining: Double,
    bullets: Int,
    explosions: Int
):
  def applyKeyPresses(keyState: KeyState): Soldier =
    this.copy(
      xVelocity = (keyState.getLeftDown(), keyState.getRightDown()) match
        case (true, false) =>
          -Soldier.WalkingSpeed
        case (false, true) =>
          Soldier.WalkingSpeed
        case _ => 0
      ,
      regularJumpQueued =
        regularJumpQueued || (yVelocity >= 0 && Soldier.Floor - y < Soldier.JumpQueueDistance && keyState
          .getUpDown()),
      superJumpQueued =
        // queue a super jump if we have one available, and we're not on the way up in our current super jump
        superJumpQueued || (keyState
          .getSpaceDown() && superJumps > 0 && !(midSuperJump && yVelocity < 0))
    )

  def completeJumps: Soldier =
    this.copy(
      yVelocity = if y == Soldier.Floor then 0 else yVelocity,
      midSuperJump = midSuperJump && y != Soldier.Floor
    )

  def applyJumps: Soldier =
    val startSuperJump = yVelocity <= 0 && superJumpQueued
    val startRegularJump =
      !startSuperJump && y == Soldier.Floor && regularJumpQueued
    this.copy(
      yVelocity =
        if startSuperJump then -Soldier.superJumpVelocity(Soldier.Floor - y)
        else if startRegularJump then -Soldier.JumpVelocity
        else yVelocity,
      superJumps = if startSuperJump then superJumps - 1 else superJumps,
      midSuperJump = midSuperJump || startSuperJump,
      regularJumpQueued =
        regularJumpQueued && !startSuperJump && !startRegularJump,
      superJumpQueued = superJumpQueued && !startSuperJump
    )

  def update(timeElapsed: Duration): Soldier =
    // force the soldier to stay in bounds
    val newX = Math.min(
      Math.max(x + xVelocity * timeElapsed.toUnit(SECONDS), -Soldier.LeftEdge),
      GameState.ScreenWidth - Soldier.RightEdge
    )
    val newY =
      Math.min(Soldier.Floor, y + yVelocity * timeElapsed.toUnit(SECONDS))
    val accel =
      if midSuperJump then Soldier.SuperJumpGravity else Soldier.Gravity
    this.copy(
      x = newX,
      y = newY,
      yVelocity = yVelocity + accel * timeElapsed.toUnit(SECONDS),
      invincibilitySecondsRemaining = Math
        .max(0, invincibilitySecondsRemaining - timeElapsed.toUnit(SECONDS)),
      ultimateInvincibilitySecondsRemaining = Math.max(
        0,
        ultimateInvincibilitySecondsRemaining - timeElapsed.toUnit(SECONDS)
      ),
      explosionSecondsRemaining =
        Math.max(0, explosionSecondsRemaining - timeElapsed.toUnit(SECONDS))
    )

  private def collectPowerUp(
      powerUp: PowerUp
  ): Either[Soldier, PowerUpInfo] =
    powerUp.info match
      case PowerUpInfo.SuperJump => Left(this.copy(superJumps = superJumps + 1))
      case PowerUpInfo.Invincibility =>
        Left(this.copy(invincibilitySecondsRemaining = 3))
      case PowerUpInfo.UltimateInvincibility =>
        Left(this.copy(ultimateInvincibilitySecondsRemaining = 5))
      case PowerUpInfo.Bullets =>
        Left(this.copy(bullets = bullets + 5))
      case PowerUpInfo.Explosion =>
        Left(this.copy(explosions = explosions + 1))
      case PowerUpInfo.DestroyAllBlocks | PowerUpInfo.ShrinkAllBlocks |
          PowerUpInfo.Points1 | PowerUpInfo.Points5 =>
        Right(powerUp.info)

  // return the Soldier after collecting all power ups, and the power ups the soldier captured
  // that affect the overall game state
  def collectPowerUps(powerUps: List[PowerUp]): (Soldier, List[PowerUpInfo]) =
    powerUps.foldLeft((this, List.empty[PowerUpInfo])) { case ((s, l), p) =>
      if s.doesCollect(p) then
        val collected = s.collectPowerUp(p)
        (collected.left.getOrElse(s), collected.toOption.toList ++ l)
      else (s, l)
    }

  private def maybeDrawForceField(context: dom.CanvasRenderingContext2D): Unit =
    val secondsRemaining = Math.max(
      invincibilitySecondsRemaining,
      ultimateInvincibilitySecondsRemaining
    )
    val isUltimate = secondsRemaining == ultimateInvincibilitySecondsRemaining
    if Soldier.forceFieldDrawWindows.exists { (lower, upper) =>
        lower < secondsRemaining && upper >= secondsRemaining
      }
    then
      context.beginPath()
      context.arc(
        hitPointX,
        centerY,
        if isUltimate then 90 else 70,
        0,
        2 * math.Pi
      )
      context.fillStyle =
        if isUltimate then "rgba(203, 167, 246, 0.6)"
        else "rgba(35, 246, 170, 0.6)"
      context.fill()

  private def maybeDrawUltimateForceField(
      context: dom.CanvasRenderingContext2D
  ): Unit =
    if Soldier.forceFieldDrawWindows.exists { (lower, upper) =>
        lower < invincibilitySecondsRemaining && upper >= invincibilitySecondsRemaining
      }
    then
      context.beginPath()
      context.arc(
        hitPointX,
        centerY,
        60,
        0,
        2 * math.Pi
      )
      context.fillStyle = "rgba(35, 246, 170, 0.6)"
      context.fill()

  private def maybeDrawExplosion(context: dom.CanvasRenderingContext2D): Unit =
    val drawWindows: List[(Double, Double)] = List(
      (0, 0.025),
      (0.075, 0.1),
      (0.15, 0.175),
      (0.225, 0.25),
      (0.3, 0.325),
      (0.375, 0.4)
    )
    if drawWindows.exists { (lower, upper) =>
        lower < explosionSecondsRemaining && upper >= explosionSecondsRemaining
      }
    then
      context.beginPath()
      context.arc(
        hitPointX,
        centerY,
        Soldier.ExplosionRadius,
        0,
        2 * math.Pi
      )
      context.fillStyle = "rgba(244, 136, 58, 0.9)"
      context.fill()

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    maybeDrawExplosion(context)
    maybeDrawForceField(context)
    context.drawImage(
      Soldier.image,
      x,
      y,
      Soldier.Width,
      Soldier.Height
    )

  private def hitPointX = x + Soldier.HitLine
  private def centerY = y + Soldier.Height / 2

  def isHit(block: Block): Boolean =
    val hitPointY = y + Soldier.Height
    val xHit = hitPointX > block.x && hitPointX < block.x + block.width
    // the soldier's foot is roughly 18 pixels wide. Because we only check his
    // center line for collision, we also give an 18 pixel buffer on the top
    // of blocks to make it feel consistent. The second check comparison has to
    // be <= because when the soldier sits on the ground,
    // hitPointY == block.y + block.height
    val yHit = hitPointY >= block.y + Math.min(
      18,
      block.height
    ) && hitPointY <= block.y + block.height
    xHit && yHit && invincibilitySecondsRemaining == 0 && ultimateInvincibilitySecondsRemaining == 0

  def doesCollect(powerUp: PowerUp): Boolean =
    Soldier.powerUpHitEdge.exists { (hitX, hitY) =>
      powerUp.eclipse.contains(hitX + x, hitY + y)
    }

  /** If the soldier is on either the left or right third, spawn on the other
    * side. If the soldier is on the left half, but moving to the left, spawn on
    * the right. If the soldier is on the right half, but moving to the right,
    * spawn on the left
    */
  def getSpawnSide(keyState: KeyState): Option[LeftOrRight] =
    val leftHalf = hitPointX < GameState.ScreenWidth / 2
    val rightHalf = !leftHalf
    val leftThird = hitPointX < GameState.ScreenWidth / 3
    val rightThird = hitPointX > 2 * GameState.ScreenWidth / 3
    if leftThird then Some(LeftOrRight.Right)
    else if rightThird then Some(LeftOrRight.Left)
    else if leftHalf && keyState.getLeftDown() && !keyState.getRightDown() then
      Some(LeftOrRight.Right)
    else if rightHalf && !keyState.getLeftDown() && keyState.getRightDown() then
      Some(LeftOrRight.Left)
    else None

  def maybeSpawnBullet: Option[Bullet] =
    if bullets > 0 then
      Some(Bullet(hitPointX, y + Soldier.Height - Bullet.Height))
    else None

  def explodedBlock(b: Block): Option[HitBonus] =
    val explosionEclipse = Eclipse(
      hitPointX - Soldier.ExplosionRadius,
      centerY - Soldier.ExplosionRadius,
      2 * Soldier.ExplosionRadius,
      2 * Soldier.ExplosionRadius
    )
    if explosionSecondsRemaining > 0 && b.explosionHitPoints.exists {
        (bX, bY) => explosionEclipse.contains(bX, bY)
      }
    then
      Some(
        HitBonus(
          b.x + b.width / 2,
          b.y + b.height / 2,
          "rgba(229, 67, 60, 1)",
          0.8
        )
      )
    else None

object Soldier:
  // if the soldier is 100 or less pixels off the ground, pressing jump will
  // queue a jump
  val JumpQueueDistance = 100
  val Gravity = 5000 // original code had 2, running at 50 fps. 5000 = 2 * 50^2
  val SuperJumpGravity =
    1250 // original code had 0.5, running at 50 fps. 1250 = 0.5 * 50^2
  val HitLine = 24 // roughly the mid line of the soldier image
  val Width = 51
  val Height = 84
  val Floor = GameState.GrassHeight - Height
  val WalkingSpeed = 400 // original code had 8. 400 = 8 * 50
  val JumpVelocity = 1400 // original code had 28. 1400 = 28 * 50
  val SuperJumpVelocity = 900 // original code had 18. 900 = 18 * 50
  val LeftEdge = 2 // the soldier's left hand in the image
  val RightEdge = 46 // the soldier's right hand in the image
  // this is a sequence of points on the outline of the soldier image. The
  // coordinates are in the original, unscaled image. We divide them by 4
  // since we scale to 1/4 in this game, and then we flip over the center line
  // of the soldier so we have points on both sides

  val ExplosionRadius = 275

  // given that we are a certain distance off the ground, how long has our
  // super jump been? This assumes we are still on the way up
  private def superJumpTimeElapsed(distanceTraveled: Double): Double =
    // solving a quadratic equation:
    // https://www.wolframalpha.com/input?i=y+%3D+v*t+-+0.5*a*t*t%2C+solve+for+t
    val v = SuperJumpVelocity
    val a = SuperJumpGravity
    val y = distanceTraveled
    (v - Math.sqrt(v * v - 2 * a * y)) / a

  // given that we are a certain distance off the ground and we are mid
  // superjump, what is our velocity?
  def superJumpVelocity(distanceTraveled: Double): Double =
    SuperJumpVelocity - SuperJumpGravity * superJumpTimeElapsed(
      distanceTraveled
    )

  def powerUpHitEdge: List[(Int, Int)] =
    val leftSide = List(
      (96, 0),
      (61, 7),
      (44, 37),
      (21, 97),
      (17, 152),
      (8, 234),
      (19, 309),
      (24, 335),
      (59, 335),
      (96, 335)
    ).map((x, y) => (x / 4, y / 4))
    val flipped = leftSide.map((x, y) => ((HitLine - x) + HitLine, y))
    leftSide ++ flipped

  val image =
    dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
  image.src = "./soldier.png"

  private val forceFieldDrawWindows: List[(Double, Double)] = List(
    (0, 0.05),
    (0.1, 0.15),
    (0.2, 0.25),
    (0.3, 0.35),
    (0.4, 0.45),
    (0.5, 0.6),
    (0.7, 0.8),
    (0.9, 1),
    (1.1, 1.2),
    (1.3, 1.4),
    (1.5, 5)
  )
