package de.scaltris.game

/**
 * Do not perform any kicks.
 */

object ConservativeKickPolicy extends KickPolicy{
  /**
   * Only accept if the new block is in a legal position. Do not eprform any kicks.
   */
  private def conservativeTest[A](stack: Stack[A], newBlock: Block[A]): Option[Block[A]] = {
    if (stack.blockIsInsideStack(newBlock) && stack.blockDoesNotCollide(newBlock))
      Some(newBlock)
    else
      None
  }

  def probeRotate[A](stack: Stack[A], oldBlock: Block[A], newBlock: Block[A]) = conservativeTest(stack, newBlock)

  def probeMove[A](stack: Stack[A], oldBlock: Block[A], newBlock: Block[A]) = conservativeTest(stack, newBlock)
}