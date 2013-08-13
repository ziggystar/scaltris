package de.scaltris.client

import de.scaltris.game._
import collection.mutable.{HashSet, Queue}

/**
 * Controls a game of Scaltris.
 */
class GameController[A](state: GameState[A], val blockSource: BlockSource[A], val kickPolicy: KickPolicy)
        extends TetrisKeyListener
                with GarbageListener{
  
  val tracker = new GameStateTracker(state.stack, state.current, state.garbage)

  private def stack = tracker.getStack
  private def current = tracker.getCurrentPiece
  private def garbage = tracker.getGarbage

  def addGameStateListener(l: GameStateListener[A]) = tracker.addGameStateListener(l)
  def removeGameStateListener(l: GameStateListener[A]) = tracker.removeGameStateListener(l)

  def addGarbage(lines: Int): Int = {
    tracker.addGarbageBlock(blockSource.garbageLines(lines, stack.width))
    0
  }

  private def rotateCurrent(angle: Int): Unit = {
    current match {
      case Some(block) => {
        kickPolicy.probeRotate(stack, block, block.rotate(angle)) match {
          case Some(newBlock) => tracker.updateCurrent(Some(newBlock))
          case _ =>
        }
      }
      case _ =>
    }
  }

  private def moveCurrent(p: Position): Unit = {
    current match {
      case Some(block) => {
        kickPolicy.probeMove(stack, block, block.translate(p)) match {
          case Some(newBlock) => tracker.updateCurrent(Some(newBlock))
          case _ =>
        }
      }
      case _ =>
    }
  }

  def rotateRight(): Unit = rotateCurrent(1)

  def rotateLeft(): Unit = rotateCurrent(3)

  def right(): Unit = moveCurrent(Position(1,0))

  def left(): Unit = moveCurrent(Position(-1,0))

  def down(): Unit = moveCurrent(Position(0,-1))

  def drop(): Unit = {
    current match {
      case Some(block) => {
        val dropped = stack.shadowOff(block)
        tracker.updateCurrent(dropped)
        tracker.lockdown(Some(blockSource.nextBlock()))
      }
    }
  }

  def start(): Unit = {
    tracker.updateCurrent(Some(blockSource.nextBlock))
  }
}