package de.scaltris.game

/**
 * Created by IntelliJ IDEA.
 * User: Thomas
 * Date: 04.09.2010
 * Time: 12:01:02
 * To change this template use File | Settings | File Templates.
 */

object Conversions {
  implicit def tuple2Position(t: Tuple2[Int, Int]) = Position(t._1, t._2)
}