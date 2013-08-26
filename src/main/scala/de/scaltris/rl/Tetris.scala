package de.scaltris.rl

import scala.util.Random
import scala.collection.immutable.BitSet


/**
 * Type-class for the mechanics of Tetris. Lines are numbered with integers.
 */
trait Tetris {
  type Move

  def numCompletedLines: Int

  def clearCompletedLines: Tetris

  def makeMove(m: Move): Tetris

  def possibleMoves: Set[Move]

  def width: Int

  def isOccupied(line: Int, col: Int): Boolean

  def highestOccupiedLine: Int

  def columnHeights: Array[Int]
}


case class InfTetris(stack: IndexedSeq[Int], pit: Stream[Int]) extends Tetris {

  import InfTetris._

  type Move = LMove

  def numCompletedLines = ???

  def clearCompletedLines = ???

  def makeMove(m: InfTetris#Move) = ???

  def possibleMoves = ???

  def width = ???

  def isOccupied(line: Int, col: Int) = ???

  def highestOccupiedLine = ???

  def columnHeights = ???
}

object InfTetris {
  final val width = 10

  def newGame(random: Random) = {
    require(width < 17)
    val mask = ~(~0 << width)
    InfTetris(IndexedSeq(), Stream.continually(mask ^ (1 << random.nextInt(width))))
  }
}