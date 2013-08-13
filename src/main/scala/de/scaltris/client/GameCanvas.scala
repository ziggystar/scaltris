package de.scaltris.client

import javax.swing.JPanel
import java.awt.{Dimension, Color, Graphics}
import de.scaltris.game._

/**
 * Created by IntelliJ IDEA.
 * User: ´Thomas
 * Date: 05.09.2010
 * Time: 12:14:18
 * To change this template use File | Settings | File Templates.
 */

class GameCanvas(var stack: Stack[Color]) extends JPanel with GameStateListener[Color] {
  var piece: Option[Block[Color]] = None
  var drawShadow: Boolean = true

  val shadowColor = Color.DARK_GRAY.brighter

  val (minBlockSize,prefBlockSize) = (5,15)

  def lightenBy(c: Color, amount: Float) = new Color(c.getColorSpace,c.getComponents(null).map(x => (1-(1-x) * (1-amount))),1f)

  def gameStateChanged(e: GameState[Color]): Unit = {
    stack = e.stack
    piece = e.current
    repaint()
  }

  override def getPreferredSize = new Dimension(stack.width * prefBlockSize, stack.height * prefBlockSize)

  override def getMinimumSize = new Dimension(stack.width * minBlockSize, stack.height * minBlockSize)

  override def paint(g: Graphics): Unit = {
    //clean background
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, this.getWidth, this.getHeight)
    val blockWidth = this.getWidth / stack.width
    val blockHeight = this.getHeight / stack.height

    def drawBlock(block: Block[Color], fillColor: Color, drawColor: Color): Unit = {
      block.components.foreach {
        component => {
          g.setColor(fillColor)
          g.fillRect(blockWidth * component.x, blockHeight * (stack.height - component.y - 1), blockWidth, blockHeight)
          g.setColor(drawColor)
          g.drawRect(blockWidth * component.x, blockHeight * (stack.height - component.y - 1), blockWidth, blockHeight)
        }
      }
    }
//    def drawBlockColored(block: Block[Color]): Unit = {
//      drawBlock(block,lightenBy(block.data,(block.bottom.toFloat / stack.height)),block.data.darker)
//    }

    //draw the blocks
    def drawBlockColored(block: Block[Color]): Unit = {
      drawBlock(block, lightenBy(block.data, (block.bottom.toFloat / stack.height)), block.data.darker)
    }
    stack.blocks.foreach(drawBlockColored)

    //draw the current piece
    piece match {
      case Some(p) => {
        drawBlockColored(p)
        val shadow = stack.shadowOff(p)
        if (shadow.isDefined) {
          //draw the shadow?
          drawBlockColored(shadow.get)
        }
      }
      case None =>
    }
  }
}