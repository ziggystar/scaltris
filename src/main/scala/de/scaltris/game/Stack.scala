package de.scaltris.game

import de.scaltris.game.Conversions._
/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 29.08.2010
 * Time: 16:38:37
 * To change this template use File | Settings | File Templates.
 */

class Stack[A](val blocks: Set[Block[A]], val width: Int, val height: Int){

  //no two blocks occupy the same position inside the stack
  assert((for(x <- 0 to width;y <- 0 to height) yield blocks.filter(_.occupies(Position(x,y))).size).max <= 1)

  /**
   * Check whether this stack is not over the top.
   */
  def isLegal = blocks.forall(blockIsInsideStack)

  def blockIsInsideStack(b: Block[A]) = b.components.forall(p => p.x >= 0 && p.x < width && p.y >= 0 && p.y < height)
  def blockDoesNotCollide(b: Block[A]) = b.components.forall(!blockAt(_).isDefined)
  def isBlockSuspended(b: Block[A]) = b.components.exists(p => p.y == 0 || blockAt(p - (0,1)).isDefined)
  def blockAt(p: Position): Option[Block[A]] = blocks.find(_.occupies(p))
  def add(aBlock: Block[A]): Stack[A] = {
    assert(blockIsInsideStack(aBlock))
    assert(isBlockSuspended(aBlock))
    assert(blockDoesNotCollide(aBlock))
    new Stack(blocks + aBlock,width,height)
  }
  def isLineCompleted(line: Int): Boolean = (0 until width).map(col => blockAt((col,line))).forall( _.isDefined )
  def completedLines: Set[Int] = (0 to height).filter(isLineCompleted).toSet
  def removeLines(lines: Set[Int]): Stack[A] = new Stack(blocks.map(_.removeLines(lines)).flatten,width,height)

  /**
   * Add some blocks as garbage. They must be placed at height 0. Note that the stack can grow beyond the top.
   */
  def addAsGarbage(garbage: Set[Block[A]]): Stack[A] = {
    //assert that the lowest block rests at height 0
    assert((for(b <- garbage; p <- b.components) yield p.y).min == 0)

    val garbageHeight = (for(b <- garbage; p <- b.components) yield p.y).max

    val shiftedBlocks = blocks.map(_.translate((0,garbageHeight + 1)))

    new Stack(shiftedBlocks ++ garbage, width, height)
  }

  def shadowOff(b: Block[A]): Option[Block[A]] = {
    if(blockIsInsideStack(b) && blockDoesNotCollide(b)){
      var shadow = b
      while(!isBlockSuspended(shadow)){
        shadow = shadow.translate((0,-1))
      }
      Some(shadow)
    }
    else
      None    
  }
}