package de.scaltris.rl.viewer

import java.lang.management.ManagementFactory

import de.scaltris.rl.controllers.linear.LinearController
import de.scaltris.rl.{FlatActionMDP, InfTetris}

import scala.util.Random

object Benchmark extends App {

  val tetris = InfTetris()
  benchmark("InfTetris random policy playouts 1000k",repeats = 10){
    () => FlatActionMDP.rollout(LinearController.fixed1(tetris),new Random(0)).drop(100000)
  }


  def benchmark(name: String, n: Int = 1, repeats: Int = 5)(f: () => Unit): Unit = {
    val times = (1 to repeats).map( _ =>
      cpuTime{ () =>
        var i = 0
        while(i < n){
          f()
          i += 1
        }
      }._2
    )

    println(s"task $name; repeating $n times each run (ms):\n\t${times.map(d => (d * 1000).toInt).mkString("; ")}")
  }

  def cpuTime[A](f: () => A): (A,Double) = {
    val threadBean = ManagementFactory.getThreadMXBean
    val start = threadBean.getCurrentThreadCpuTime
    var r = f()
    (r,(threadBean.getCurrentThreadCpuTime - start).toDouble * 1e-9)
  }
}
