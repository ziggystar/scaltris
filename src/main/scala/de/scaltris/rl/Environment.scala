package de.scaltris.rl

import de.scaltris.game.{Position, Block}

import scala.util.Random

/**
 * @author Thomas Geier
 * @since 11/20/13
 */

trait FlatActionMDP {
  type State
  type Action

  def initialState(r: Random): State
  def act(state: State, action: Action, r: Random): (State,Double)
  def actions(state: State): IndexedSeq[Action]
}

case class InfTetris(width: Int = 10, maxHeight: Int = 22,
                     minHeight: Int = 4,
                     clearReward: Int => Double = _ => 1,
                     newLineReward: Double = 1,
                     topOutLineReward: Double = -10) extends FlatActionMDP {
  type Tetromino = Int

  /**First is the stack, second is next piece. */
  type State = (Stack,Tetromino)

  type PartialStack = Array[Int]

  val fullLine: Int = (0 until width).map(1 << _).reduce(_|_)

  /** index with [Tetromino][rotations][x-offset]. Pieces touch the bottom.*/
  val blocks: IndexedSeq[IndexedSeq[IndexedSeq[Block[Unit]]]] = (0 to 6).map { tetro =>
    val b = Block.stdShape(tetro,Position(0,0),())
    (0 to 3).map(b.rotate).distinct.map{rotated =>
      (0 to (width-1)).map(xoff =>
        rotated.translate(Position(xoff,0))
      ).filterNot(b => b.components.map(_.x).max < width)
    }
  }

  case class Action(rotation: Int, xOffset: Int)

  case class Stack(rows: Array[Int]) extends AnyVal {
  def topRow: Int = rows.zipWithIndex.filterNot(_._1 == 0).map(_._2).max
    def fullLines = rows.zipWithIndex.filter(li => isFullLine(li._1)).map(_._2)
    def isFullLine(l: Int): Boolean = (l & fullLine) == fullLine
    /** @return Second is number of added garbage lines. */
    def fillToMin(r: Random): (Stack,Int) = {
      val requiredLines = topRow - minHeight
      val newStack = Iterator.iterate(this)(_.addGarbageLineUnsafe(r)).drop(requiredLines).next()
      (newStack,requiredLines)
    }
    /** Adds one line of garbage with a random hole at the bottom. */
    private def addGarbageLineUnsafe(r: Random): Stack = Stack((fullLine & ~(1 << r.nextInt(width))) +: rows.init)
  }
  object Stack{
    def empty: Stack = new Stack(Array.fill(maxHeight)(0))
  }


  def randomTetromino(r: Random): Tetromino = r.nextInt(7)

  override def initialState(r: Random): State = (Stack.empty.fillToMin(r)._1, randomTetromino(r))

  override def actions(state: State): IndexedSeq[Action] = for {
    (offs,rot) <- blocks(state._2).zipWithIndex
    (_,off) <- offs.zipWithIndex
  } yield Action(rot,off)

  override def act(state: State, action: Action, r: Random): (State, Double) = ???
}