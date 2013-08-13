package de.scaltris.client

import java.awt.{AWTEvent, Toolkit}
import java.awt.event.{AWTEventListener, KeyEvent, KeyListener}
import collection.mutable.{SynchronizedMap, HashSet}

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 12.09.2010
 * Time: 21:02:14
 * To change this template use File | Settings | File Templates.
 */

class KeyboardInputControl( val handlers: Map[Int,TetrisKeyListener=>Unit]) {
  var delay = 150
  var repeat = 70

  private val keyListeners = new HashSet[TetrisKeyListener]()

  //maps from held down keycodes onto ms, after which they'll fire again
  private val keyDecay = new collection.mutable.HashMap[Int,Int]() with SynchronizedMap[Int,Int]

  private val lastEventByKey = new collection.mutable.HashMap[Int,KeyEvent]() with SynchronizedMap[Int,KeyEvent]

  val evtListener = new AWTEventListener {
    def eventDispatched(p1: AWTEvent): Unit = {
      val ke = p1.asInstanceOf[KeyEvent]

      ke.getID match{
        case KeyEvent.KEY_PRESSED => keyPressed(ke)
        case KeyEvent.KEY_RELEASED => keyReleased(ke)
        case _ =>
      }
    }
  }

  {
    Toolkit.getDefaultToolkit().addAWTEventListener(evtListener, AWTEvent.KEY_EVENT_MASK);
  }

  def addTetrisKeyListener(l: TetrisKeyListener) = keyListeners.add(l)
  def removeTetrisKeyListener(l: TetrisKeyListener) = keyListeners.remove(l)

  private def keyReleased(p1: KeyEvent){
    lastEventByKey.put(p1.getKeyCode, p1)
  }

  private def triggerKey(keycode: Int) { keyListeners.foreach(handlers(keycode)) }

  private def keyPressed(p1: KeyEvent){
    val code = p1.getKeyCode

    //check if we're interested in this key press
    if (handlers.contains(code)){
      if( !lastEventByKey.contains(code) || ((p1.getWhen - lastEventByKey(code).getWhen) > 2)) {
        //an old release registered
        triggerKey(code)
        keyDecay += code -> delay
      }
      lastEventByKey.remove(code)
    }
  }

  /**
   * Argument says ms since last call to this method.
   */
  def externalTrigger(diff: Int){
    //retain only the possible repeat-release events
    var now = System.currentTimeMillis
    val (legitimate,repeats) = lastEventByKey.partition{case (key,evt) => (now - evt.getWhen) > 5}
    lastEventByKey --= legitimate.keys

    //clear the decays of the legitimate keys
    keyDecay --= legitimate.keys

    //process legitimate release events
    val update = for( (keycode,remain) <- keyDecay) yield {
      val newremain  = remain - diff
      if( newremain < 0 ){
        triggerKey(keycode)
        keyDecay.update(keycode, newremain + repeat)
      } else
        keyDecay.update(keycode, newremain)
    }
  }
}