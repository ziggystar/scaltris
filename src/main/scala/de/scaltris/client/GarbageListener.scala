package de.scaltris.client

import de.scaltris.game._
import collection.mutable.Queue

/**
 * Controlls a game of Scaltris.
 */

trait GarbageListener{
  /**
   * Add garbage to a game.
   * @param holes A list which holds the holes in each row of added garbage from left. The added garbage amounts to holes.size lines.
   * The first line in holes is the topmost to be added.
   * @return The piece number, after which the garbage is added to the incoming box.
   */
  def addGarbage(holes:Int): Int
}



