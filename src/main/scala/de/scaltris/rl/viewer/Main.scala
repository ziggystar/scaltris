package de.scaltris.rl.viewer

import java.awt._
import javax.swing.GroupLayout.Alignment
import javax.swing._

import de.scaltris.rl.controllers.linear.LinearController
import de.scaltris.rl.controllers.linear.ce.CrossEntropyMethod
import de.scaltris.rl.{Policy, SARSA, FlatActionMDP, InfTetris}
import rx.lang.scala.{Observer, Subject, Observable}

import scala.concurrent.duration.Duration
import scala.swing.FlowPanel
import scala.util.Random

object Main {
  def main(args: Array[String]) {
    val window = new JFrame("RL Tetris")

    val tetris = InfTetris(minHeight = 4, newLineReward = _ * 100)
    val state = Subject[tetris.Stack]()

    var rand = new Random(0)

    val ce = new CrossEntropyMethod(tetris,numSamples = 500, nBest = 300, sampleLength = 1000)(tetris.allFeatures)

    println(ce)

    val init: Iterator[ce.Distribution] = Iterator
      .iterate(new ce.IndependentNormal(IndexedSeq.fill(ce.features.size)((0d,5d)),rand): ce.Distribution)(_.next(rand))

//    val dists = init.take(15).map{ceDist =>
//      println(ceDist)
//      ceDist
//    }.toIndexedSeq

    val polPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT))

    val fixedPolicy: LinearController = new LinearController(tetris)(
      Seq(
        tetris.PotentialEnergy -> -4,
        tetris.VTransitions -> -10,
        tetris.MaxHeight -> 1,
        tetris.BlockCount -> -3,
        tetris.DistinctHeights -> 3,
        tetris.Hops -> -2
      )
    )

    polPanel.add(new PolicyViewer(fixedPolicy))
//    polPanel.add(new PolicyViewer(dists.last.controller))

    window.getContentPane.add(polPanel, BorderLayout.CENTER)
    window.getContentPane.add(new JLabel("RL Tetris"), BorderLayout.NORTH)

    window.pack()
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    window.setVisible(true)
  }

  def fromIntervalled[A](xs: Iterable[A], interval: Duration, delay: Duration = Duration(0, "s")): Observable[A] = {
    val values = Observable.from(xs)
    Observable.interval(interval).delay(delay).zip(values)
      .map(_._2)
  }
}

class PolicyViewer(val policy: Policy {val mdp: InfTetris},
                   interval: Duration = Duration("1s"), random: Random = new Random(0))
  extends JPanel() {
  this.setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))

  val behavior = Main.fromIntervalled(FlatActionMDP.rolloutWithAction(policy, random).toIterable, interval, delay = Duration(1, "s"))
  val reward: Observable[Double] = behavior.map(_._1._2)

  //num, sum, sum of squar2es
  val rewardAccum: Observable[(Int, Double, Double)] =
    reward.scan((0,0d,0d)){case ((n,sum,sumsq),r) => (n+1,sum + r, sumsq + r*r)}
  val stackView = new StackViewer(policy.mdp)(behavior)

  this.add(stackView)
  this.add(new RxLabel(rewardAccum.sample(Duration("1 s")).map{case (n,s,ss) => f"avg ${s/n}%.2f"}))

}

class StackViewer(val rules: InfTetris)(val state: Observable[((InfTetris#State,Double),InfTetris#Action)]) extends RxDrawable {
  val cellSize: Int = 10
  this.setPreferredSize(new Dimension(cellSize * rules.width, cellSize * rules.maxHeight))

  state.subscribe{s =>
    drawStack(s._1._1,s._2)
    repaint()
  }
  
  def drawStack(state: InfTetris#State, action: InfTetris#Action): Unit = {
    def drawBlock(x: Int, y: Int, color: Color): Unit = {
      g2d.setColor(color)
      g2d.fillRect(x * cellSize, (rules.maxHeight - y - 1) * cellSize, cellSize, cellSize)
    }
    val (stack,piece) = state
    //draw stack
    for{
      x <- 0 until rules.width
      y <- 0 until rules.maxHeight
    } {
      drawBlock(x,y,if(stack.isSet(x,y)) Color.BLACK else Color.WHITE)
    }
    val block: InfTetris#Stack = action.getBlock(piece)
    //Draw shadow of next piece
    val contact = stack.asInstanceOf[rules.Stack].contactHeight(block.asInstanceOf[rules.Stack])
    for{
      (x,y) <- block.occupations
    } {
      drawBlock(x,y+contact,Color.GRAY)
    }
  }
}

class RxLabel(obs: Observable[String]) extends JLabel{
  val subscription = obs.subscribe { s =>
    this.setText(s)
    this.repaint()
  }
}

class RxDrawable extends JComponent {
  @volatile var image: Image = null
  @volatile var g2d: Graphics2D = null
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