package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

def animate(
    rng: util.Random,
    context: dom.CanvasRenderingContext2D,
    previousFrameTimestamp: Option[Double],
    gameState: GameState,
    keyState: KeyState
): Double => Unit = { (msTimestamp: Double) =>
  val timeElapsed: Duration = previousFrameTimestamp match
    case None       => Duration.Zero
    case Some(prev) => Duration.fromNanos(1000000 * (msTimestamp - prev))
  assert(timeElapsed >= Duration.Zero)
  context.beginPath()
  // draw the sky
  context.fillStyle = "#5FA6E7"
  context.rect(
    0,
    0,
    GameState.ScreenWidth,
    GameState.GrassHeight
  ) // x, y, width, height
  context.fill()
  gameState.draw(context)
  // draw the grass
  context.beginPath()
  context.fillStyle = "rgba(52, 131, 47, 1)"
  context.rect(
    0,
    GameState.GrassHeight,
    GameState.ScreenWidth,
    GameState.ScreenHeight - GameState.GrassHeight
  )
  context.fill()
  if !gameState.isOver then
    dom.window.requestAnimationFrame(
      animate(
        rng,
        context,
        Some(msTimestamp),
        gameState.update(msTimestamp / 1000, timeElapsed, keyState, rng),
        keyState
      )
    )
  else println(s"Time elapsed: ${msTimestamp / 1000}")
}

enum LeftOrRight:
  case Left, Right
  def flip = this match
    case Left  => Right
    case Right => Left

class KeyState(
    private var leftDown: Boolean,
    private var rightDown: Boolean,
    private var upDown: Boolean,
    private var spaceDown: Boolean,
    private var xClicked: Boolean,
    private var zClicked: Boolean
):
  def getLeftDown() = leftDown
  def getRightDown() = rightDown
  def getUpDown() = upDown
  def getSpaceDown() = spaceDown
  def processXClick(): Boolean =
    val current = xClicked
    xClicked = false
    current
  def processZClick(): Boolean =
    val current = zClicked
    zClicked = false
    current
  def processKeyEvent(e: dom.KeyboardEvent, pressedDown: Boolean): Unit =
    e.keyCode match
      case 37 => leftDown = pressedDown
      case 38 => upDown = pressedDown
      case 39 => rightDown = pressedDown
      case 32 => spaceDown = pressedDown
      case 88 => xClicked |= pressedDown
      case 90 => zClicked |= pressedDown
      case x  => ()

@main def main(): Unit =
  val rng = util.Random()
  val context: dom.CanvasRenderingContext2D = dom.document
    .querySelector("canvas")
    .asInstanceOf[dom.html.Canvas]
    .getContext("2d")
    .asInstanceOf
  val canvas: dom.HTMLCanvasElement = context.canvas
  canvas.width = GameState.ScreenWidth
  canvas.height = GameState.ScreenHeight
  val keyState = KeyState(false, false, false, false, false, false)
  dom.window.addEventListener("keydown", keyState.processKeyEvent(_, true))
  dom.window.addEventListener("keyup", keyState.processKeyEvent(_, false))
  dom.window.requestAnimationFrame(
    animate(
      rng,
      context,
      None,
      GameState(
        Soldier(
          GameState.ScreenWidth / 2 - Soldier.HitLine,
          0,
          0,
          0,
          0,
          false,
          false,
          false,
          0,
          0,
          0,
          0,
          0
        ),
        0,
        List.empty,
        List.empty,
        List.empty,
        List.empty,
        List.empty
      ),
      keyState
    )
  )
