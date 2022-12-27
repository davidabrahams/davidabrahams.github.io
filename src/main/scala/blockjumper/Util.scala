package blockjumper

import org.scalajs.dom

object Util:
  def drawCircleWithText(
      context: dom.CanvasRenderingContext2D,
      centerX: Double,
      centerY: Double,
      radius: Double,
      fillColor: String,
      text: String,
      fontSize: Int,
      fontColor: String
  ): Unit =
    context.beginPath()
    context.arc(centerX, centerY, radius, 0, 2 * math.Pi)
    context.fillStyle = fillColor
    context.fill()
    fillText(
      context,
      text,
      centerX,
      centerY,
      fontSize,
      fontColor
    )

  def fillText(
      context: dom.CanvasRenderingContext2D,
      text: String,
      centerX: Double,
      centerY: Double,
      fontSize: Int,
      fontColor: String
  ): Unit =
    val fontLineHeight = 5 * fontSize / 6
    val lines = text.split("\n")
    val apxFontHeight = fontLineHeight * lines.length
    // the text is drawn based on the bottom left coordinate. The logic makes
    // the center of the text in the y-axis equal centerY
    var currentY = centerY + fontLineHeight - apxFontHeight / 2
    context.fillStyle = fontColor
    context.font = s"${fontSize}px sans-serif";
    lines.foreach { l =>
      context.fillText(l, centerX - context.measureText(l).width / 2, currentY)
      currentY += fontLineHeight
    }
