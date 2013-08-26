package de.scaltris.rl

import scala.collection.immutable.BitSet.BitSet1
import scala.collection.immutable.BitSet

/**
 * Encodes a x-translated and rotated piece in one Long. Maximum x of a single block is 16.
 * First two bytes are line y=0, second two bytes are line y=1 and so on.
 */
case class LMove(data: Long) extends AnyVal {
  def asSet: Set[(Int, Int)] = new BitSet1(data).map {
    x =>
      (x % 16, x / 16)
  }
}

object LMove {
  def fromSet(block: Set[(Int, Int)]): LMove = {
    val bs = BitSet(block.map {
      case (x, y) => x + 16 * y
    }.toSeq: _*)
    LMove(bs.toBitMask(0))
  }
}
