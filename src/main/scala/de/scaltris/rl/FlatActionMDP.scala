package de.scaltris.rl

import scala.collection.mutable
import scala.util.Random

/**
 * @author Thomas Geier
 * @since 11/20/13
 */

trait FlatActionMDP {
  type State
  type Action

  def initialState(r: Random): State
  def act(state: State, action: Action, r: Random): (State,Double)
  def actions(state: State): IndexedSeq[Action]
  def discount: Double = 0.9

  trait Feature {
    def compute(state: State): Double
  }
}

object FlatActionMDP {
  def randomPolicy(_mdp: FlatActionMDP): Policy = new Policy {
    val mdp = _mdp
    override def getAction(state: mdp.State, r: Random): mdp.Action = {
      val acts = mdp.actions(state)
      acts(r.nextInt(acts.size))
    }
  }

  def rollout(policy: Policy, r: Random): Iterator[(policy.mdp.State, Double)] =
    Iterator.iterate(policy.mdp.initialState(r) -> 0d){case (s,_) => policy.mdp.act(s,policy.getAction(s,r),r)}

  def rolloutWithAction(policy: Policy, r: Random): Iterator[((policy.mdp.State, Double),policy.mdp.Action)] = {

    val initState = policy.mdp.initialState(r)

    Iterator.iterate(((initState,0d),policy.getAction(initState,r))){case ((s,_),a) =>
      val nsr@(nextState,_) = policy.mdp.act(s,a,r)
      val nextAction = policy.getAction(nextState,r)
      (nsr,nextAction)
    }
  }

  def randomPlayout(mdp: FlatActionMDP,r: Random): Iterator[(mdp.State, Double)] =
    rollout(randomPolicy(mdp), r).asInstanceOf[Iterator[(mdp.State, Double)]]
}

class SARSA[F](val mdp: FlatActionMDP, defaultValue: Double = 0)(_extract: FlatActionMDP#State => F) extends Policy {
  val extract = _extract.asInstanceOf[mdp.State => F]
  val q: mutable.HashMap[(F,mdp.Action),Double] = new mutable.HashMap()
  def train(steps: Int, learningRate: Double, random: Random): Unit = {
    var s = mdp.initialState(random)
    var fs = extract(s)
    var a = getAction(s,random)
    var qsa = q.getOrElse((fs,a),defaultValue)
    var i = 0
    while(i < steps){
      val (s2,r) = mdp.act(s,a,random)
      val fs2 = extract(s2)
      val a2 = getAction(s2, random)
      val qs2a2 = q.getOrElse((fs2,a2),defaultValue)
      //update q
      q((fs,a)) = qsa + learningRate*(r + mdp.discount*qs2a2  - qsa)
      s = s2
      fs = fs2
      a = a2
      qsa = qs2a2
      i += 1
    }
  }

  override def getAction(state: mdp.State, r: Random): mdp.Action = {
    val f = extract(state)
    mdp.actions(state).maxBy(a => q.getOrElse((f,a),defaultValue))
  }
}

object SARSA {
  def apply[F](mdp: FlatActionMDP, defaultValue: Double = 0)(extract: mdp.State => F) =
    new SARSA(mdp, defaultValue)(extract.asInstanceOf[FlatActionMDP#State => F])
}

trait Policy {
  val mdp: FlatActionMDP
  def getAction(state: mdp.State, r: Random): mdp.Action
}