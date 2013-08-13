/*
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 04.02.11
 * Time: 20:49
 */
package de.scaltris.game

import java.util.{TimerTask, Timer}
import collection.mutable.HashMap

/**
 * A game loop that calls its handlers every period ms. Argument to handlers is elapsed time since last call.
 */
class GameLoop(val period: Int){
  val handlers = new collection.mutable.Queue[Int => Unit]

  val timer = new Timer("GameLoop Thread", true)

  private var last = System.currentTimeMillis

  val thread = new Thread(new Runnable{
    def run() {gameLoop()}
  }, "GameLoop")

  var nextLoop = System.currentTimeMillis

  thread.setDaemon(true)
  thread.start()

  private def gameLoop() {
    var l = 0
    var m = new HashMap[Int,Int]()

    while(true){
      l = l + 1
      if(l % 1000 == 0){
        println(m.toList.sorted.mkString("%d:\n".format(l),"\n","---"))
        m.clear()
      }

      val now = System.currentTimeMillis
      val diff = ((now - last)).toInt
      last = now
      nextLoop = nextLoop + period

      m.put(diff, m.getOrElse(diff,0) + 1)

      //call handlers
      handlers.foreach(_.apply(diff))

      //schedule new
      Thread.sleep( (nextLoop - System.currentTimeMillis).max(0) )
    }
  }

  def addHandler(h: Int => Unit) {
    handlers += h
  }
}