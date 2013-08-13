package de.scaltris.client

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 11.09.2010
 * Time: 22:21:23
 * To change this template use File | Settings | File Templates.
 */

trait GameStateListener[A]{
  def gameStateChanged(e: GameState[A]): Unit
}