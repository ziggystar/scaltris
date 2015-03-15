package de.scaltris.game

import de.scaltris.game.Conversions._
import annotation.tailrec

/**
 * A Block is simply a group of dots that move and rotate as a group.
 * It also has a center around which it will rotate.
 */

class Block[A](
        val components: Set[Position],
        val data: A,
        val center: Position,
        val rotateOnIntersection: Boolean = false){
  def occupies( p: Position ): Boolean = components.contains(p)
  
  def collidesWith(other: Block[A]): Boolean = components.exists(other.occupies)

  def bottom = components.view.map(_.y).min
  def top = components.view.map(_.y).max

  /**
   * Remove all components with given y value. Every component above this line gets shifted down by one.
   * If this operation removes all components from this block, then return None.
   */
  def removeLines(_lines: Set[Int]): Option[Block[A]] = {
    //transform the lines, so we take into account when removing line 0 all following lines shift one down
    val lines = _lines.toList.sorted.zipWithIndex.map{case (i,j) => i - j}

    val newComponents = lines.foldLeft(components){(c,line) =>
      c.collect{
        case Position(x,y) if (line < y) => Position(x,y-1)
        case Position(x,y) if (line > y) => Position(x,y)
      }
    }

    if(newComponents.isEmpty)
      None
    else
      Some(new Block(newComponents.toSet, data, center, rotateOnIntersection))
  }

  def translate( p: Position ) = new Block(components.map(_.translate(p)), data, center.translate(p), rotateOnIntersection)

  /**
   * Rotates in steps of 90° clockwise.
   */
  def rotate( angle: Int ) = new Block(
    components.map(_.rotateAround(center,angle,rotateOnIntersection)),
    data,
    center,
    rotateOnIntersection)
}

object Block{
  import de.scaltris.game.Conversions._

  def stdShape[A](which: Int, pos: Position, data: A): Block[A] = {
    val components: (Set[Position], (Int,Int), Boolean) = which match {
      case 0 => (Set((1,3),(1,2),(1,1),(1,0)), (1,2), true)
      case 1 => (Set((0,1),(1,1),(2,1),(1,0)), (1,1), false)
      case 2 => (Set((1,2),(1,1),(1,0),(2,0)), (1,1), false)
      case 3 => (Set((1,2),(1,1),(1,0),(2,0)), (1,1), false)
      case 4 => (Set((0,0),(1,0),(1,1),(2,1)), (1,1), false)
      case 5 => (Set((0,1),(1,0),(1,1),(2,0)), (1,1), false)
      case 6 => (Set((0,0),(0,1),(1,0),(1,1)), (1,1), true)
    }

    new Block(components._1.map(_.translate(pos)), data, components._2.translate(pos), components._3)
  }
}