package de.scaltris.client

import de.scaltris.game._
import collection.mutable.Queue

/**
 * Used to channel the player input.
 */
trait TetrisKeyListener{
  def left(): Unit
  def right(): Unit
  def drop(): Unit
  def down(): Unit
  def rotateLeft(): Unit
  def rotateRight(): Unit
}