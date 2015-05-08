import fr.iscpif.schelling.quantity.initial.RandomState
import fr.iscpif.schelling.quantity.move.{RandomMoves, Unsatisfied}
import fr.iscpif.schelling.quantity.{State, Schelling}

import scala.util.Random

object Simulation extends App {

  implicit val rng = new Random


  val simulation = new Schelling with RandomState with RandomMoves {
    override def size: Int = 100
    override def greenRatio: Double = 0.3
    override def redRatio: Double = 0.5
    override def maxCapacity: Int = 100
    override def similarWanted: Double = 0.4
  }

  for {
    (state, step) <- simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
    println(s"Step $step: $unsatisfied unsatisfied")
  }

}
