package de.scaltris.client

import de.scaltris.game._
import javax.swing.{JLabel, JFrame, JPanel}
import java.awt.{BorderLayout, Color}
import java.awt.event.{KeyAdapter, KeyEvent}

/**
 * Created by IntelliJ IDEA.
 * User: ´Thomas
 * Date: 05.09.2010
 * Time: 12:13:20
 * To change this template use File | Settings | File Templates.
 */

object ScaltrisSwing {
  def main(args: Array[String]): Unit = {
    val window = new JFrame("Scaltris")
    val tetrisPanel = new GameCanvas(new Stack(Set.empty, 10, 20))

    val controller = new GameController[Color](
      new GameState[Color](new Stack[Color](Set.empty, 10, 20), None, Nil),
      new RandomBlockSource(123),
      ConservativeKickPolicy
      )

    controller.addGameStateListener(tetrisPanel)

    val keyControl = new KeyboardInputControl(
      Map(
        KeyEvent.VK_K -> {l: TetrisKeyListener => l.right()},
        KeyEvent.VK_N -> {l: TetrisKeyListener => l.left()},
        KeyEvent.VK_J -> {l: TetrisKeyListener => l.drop()},
        KeyEvent.VK_D -> {l: TetrisKeyListener => l.rotateLeft()},
        KeyEvent.VK_V -> {l: TetrisKeyListener => l.rotateRight()},
        KeyEvent.VK_F -> {l: TetrisKeyListener => l.down()}
        )
      )

    window.addKeyListener(new KeyAdapter(){
      override def keyPressed(p1: KeyEvent) {
        if(p1.getKeyCode == KeyEvent.VK_L)
          controller.addGarbage(2)
      }
    })

    val gameloop = new GameLoop(20)
    gameloop.addHandler(keyControl.externalTrigger)

    keyControl.addTetrisKeyListener(controller)

    controller.start()

    window.getContentPane.add(tetrisPanel)
    window.getContentPane.add(new JLabel("Scaltris"), BorderLayout.NORTH)
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    window.pack()
    window.setVisible(true)
  }
}