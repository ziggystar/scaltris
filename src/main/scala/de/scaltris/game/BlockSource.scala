package de.scaltris.game

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 11.09.2010
 * Time: 22:45:13
 * To change this template use File | Settings | File Templates.
 */

trait BlockSource[A]{
  /**
   * Get the next block and advance the source.
   */
  def nextBlock(): Block[A]

  /**
   * Get the nth next block without advancing.
   * @param n The block to peek. Zero denotes the next block.
   */
  def peekBlock(n: Int): Block[A]

  def garbageLines(numLines: Int, width: Int): Set[Block[A]]
}