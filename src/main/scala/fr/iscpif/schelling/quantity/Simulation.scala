package fr.iscpif.schelling.quantity

import initial._
import move._
import metric._

import scala.util.Random

object Simulation extends App {

  implicit val rng = new Random


  val simulation = new Schelling with RandomState with RandomMoves {
    override def size: Int = 100
    override def greenRatio: Double = 0.5
    override def redRatio: Double = 0.5
    override def maxCapacity: Int = 100
    override def similarWanted: Double = 0.5
  }

  for {
    (state, step) <- simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
       println(s"Step $step: Number of unsatisfied: $unsatisfied, dissimilarity index D: ${dissimilarity(state.matrix, Green, Red)}, entropy index H: ${entropy(state.matrix, Green, Red)}")
     }

}
