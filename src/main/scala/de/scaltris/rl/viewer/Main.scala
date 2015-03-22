package de.scaltris.rl.viewer

import java.awt._
import javax.swing._

import de.scaltris.rl.controllers.linear.LinearController
import de.scaltris.rl.{SARSA, FlatActionMDP, InfTetris}
import rx.lang.scala.{Observer, Subject, Observable}

import scala.concurrent.duration.Duration
import scala.util.Random

object Main {
  def main(args: Array[String]) {
    val window = new JFrame("RL Tetris")

    val tetris = InfTetris(minHeight = 0)
    val state = Subject[tetris.Stack]()



    var rand = new Random(0)
    val policy = new LinearController(tetris)(Seq(
      tetris.PotentialEnergy -> -5
    ))

    val states = FlatActionMDP.rollout(policy, rand)
    val stateObs = Observable.interval(Duration("100ms")).delay(Duration("1s")).map { _ =>
      states.next()
    }
//    stateObs.subscribe(println(_))

    window.getContentPane.add(new StackViewer(tetris)(stateObs.map(_._1.asInstanceOf[tetris.State]._1)), BorderLayout.CENTER)
    window.getContentPane.add(new JLabel("RL Tetris"), BorderLayout.NORTH)

    window.pack()
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    window.setVisible(true)
  }
}

class StackViewer(val rules: InfTetris)(val state: Observable[InfTetris#Stack]) extends RxDrawable {
  val cellSize: Int = 10
  this.setPreferredSize(new Dimension(cellSize * rules.width, cellSize * rules.maxHeight))

  state.subscribe{s =>
    drawStack(s)
    repaint()
  }
  
  def drawStack(stack: InfTetris#Stack): Unit = {
    for{
      x <- 0 until rules.width
      y <- 0 until rules.maxHeight
    } {
      g2d.setColor(if(stack.isSet(x,y)) Color.BLACK else Color.WHITE)
      g2d.fillRect(x * cellSize, (rules.maxHeight - y - 1) * cellSize, cellSize, cellSize)
    }
  }
}


class RxDrawable extends JComponent {
  var image: Image = null
  var g2d: Graphics2D = null
  setDoubleBuffered(false)
  override def paintComponent(g: Graphics) {
    if (image == null) {
      image = createImage(getSize().width, getSize().height)
      g2d = image.getGraphics().asInstanceOf[Graphics2D]
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      clear()
    }
    g.drawImage(image, 0, 0, null)
  }
  def clear() {
    g2d.setPaint(Color.white)
    g2d.fillRect(0, 0, getSize().width, getSize().height)
    g2d.setPaint(Color.black)
    repaint()
  }
}