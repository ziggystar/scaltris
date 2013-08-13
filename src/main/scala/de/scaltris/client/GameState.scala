package de.scaltris.client

import de.scaltris.game.{Block, Stack}

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 11.09.2010
 * Time: 22:22:16
 * To change this template use File | Settings | File Templates.
 */

case class GameState[A](stack: Stack[A], current: Option[Block[A]], garbage: List[Int])