package de.scaltris.game

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 11.09.2010
 * Time: 22:52:39
 * To change this template use File | Settings | File Templates.
 */

trait KickPolicy{
  def probeMove[A](stack: Stack[A], oldBlock: Block[A], newBlock: Block[A]): Option[Block[A]]
  def probeRotate[A](stack: Stack[A], oldBlock: Block[A], newBlock: Block[A]): Option[Block[A]]
}