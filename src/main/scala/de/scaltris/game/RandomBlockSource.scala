package de.scaltris.game

import java.awt.Color

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 12.09.2010
 * Time: 21:26:28
 * To change this template use File | Settings | File Templates.
 */

class RandomBlockSource(seed: Long) extends BlockSource[Color]{
  private val randBlock = new java.util.Random(seed)
  private val randGarbage = new java.util.Random(seed)

  
  val col = Map(
    0 -> Color.BLUE,
    1 -> Color.CYAN,
    2 -> Color.RED,
    3 -> Color.YELLOW,
    4 -> Color.ORANGE,
    5 -> Color.GREEN,
    6 -> Color.PINK
    )
  val garbageColor = Color.gray

  def peekBlock(n: Int): Block[Color] = null
  def nextBlock(): Block[Color] = {
    val n = randBlock.nextInt.abs % 7
    Block.stdShape(n, Position(5,15), col(n))
  }

  /**
   * Create a block of garbage, that has the hole at a spot defined by the next garbage random number.
   *
   * @param Amount of lines to generate. All lines will have the hole at the same position.
   */
  def garbageLines(numLines: Int, width: Int) = {
    val hole = randGarbage.nextInt(width)
    val pos = for{
      y <- 0 until numLines
      x <- 0 until width
      if( x != hole)
    } yield Position(x,y)
    Set(new Block(pos.toSet,garbageColor,Position(0,0),false))
  }
}