package de.scaltris.rl

import scala.util.Random
import scala.collection.immutable.BitSet.BitSet1

/**
 * Type-class for the mechanics of Tetris. Lines are numbered with integers.
 */
trait Tetris {
  type Move
  def numCompletedLines: Int
  def clearCompletedLines: Tetris
  def isMoveLegal(m: Move): Boolean
  def makeMove(m: Move): Tetris

  def possibleMoves: Set[Move]

  def width: Int
  def isOccupied(line: Int, col: Int): Boolean
  def highestOccupiedLine: Int
  def columnHeights: Array[Int]
}

case class InfiniteTetris(baseLine: Int, stack: Array[Int], random: Random) extends Tetris{
  final val width = 10

  type Move = Long
}

/**First two bytes are line y=0, second two bytes are line y=1 and so on. */
class LMove(data: Long) extends AnyVal {
  def asSet: Set[(Int,Int)] = for{
    y <- 0 until 4
    shiftedLong <- data << (y*16) & 0xFFFF
    x <- new BitSet1(shiftedLong)
  } yield (x,y)
}
