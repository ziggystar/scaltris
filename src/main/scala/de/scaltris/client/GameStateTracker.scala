package de.scaltris.client

import de.scaltris.game._
import collection.mutable.HashSet

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 12.09.2010
 * Time: 20:01:57
 * To change this template use File | Settings | File Templates.
 */

class GameStateTracker[A](private var stack: Stack[A], private var current: Option[Block[A]], private var garbage: List[Int]){
  def state = GameState(stack, current, garbage)

  def getStack = stack
  def getCurrentPiece = current
  def getGarbage = garbage

  val gameStateListeners = new HashSet[GameStateListener[A]]()

  def addGameStateListener(l: GameStateListener[A]) = gameStateListeners.add(l)
  def removeGameStateListener(l: GameStateListener[A]) = gameStateListeners.remove(l)

  def updateCurrent(newCurrent: Option[Block[A]]) = {
    current = newCurrent
    changed()
  }

  /**
   * Locks the new piece in place.
   */
  def lockdown(next: Option[Block[A]]): Unit = {
    stack = stack.add(current.get)
    current = next
    this.clearLines()

    changed()
  }

  /**
   * Looks for full lines and removes them.
   */
  def clearLines(): Unit = {
    val lines = stack.completedLines
    println(lines)
    if(!lines.isEmpty){
      stack = stack.removeLines(lines)
    }

    changed()
  }

  def addGarbageBlock(block: Set[Block[A]]) {
    stack = stack.addAsGarbage(block)
    changed()
  }

  private def changed(): Unit = gameStateListeners.foreach{_.gameStateChanged(state)}
}