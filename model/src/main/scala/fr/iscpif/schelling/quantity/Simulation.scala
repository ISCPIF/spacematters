package fr.iscpif.schelling.quantity

import java.io.File

import initial._
import move._
import metric._

import scalax.io.Resource

import scala.util.Random

object Simulation extends App {

  implicit val rng = new Random

  val simulation = new Schelling with RandomState with RandomMoves {
    override def size: Int = 50
    override def greenRatio: Double = 0.5
    override def redRatio: Double = 0.35
    override def maxCapacity: Int = 50
    override def similarWanted: Double = 0.4
  }

  val file = new File("/tmp/result.csv")
  file.delete()

  val output = Resource.fromFile(file)

  for {
    (state, step) <- simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
    println(s"Step $step: # of unsatisfied: $unsatisfied, Dissimilarity D: ${"%.3f".format(dissimilarity(state.matrix, Green, Red))}, Entropy H: ${"%.3f".format(entropy(state.matrix, Green, Red))}, Exposure Reds to Greens :${"%.3f".format(exposureOfColor1ToColor2(state.matrix, Red, Green))}, Isolation Reds :${"%.3f".format(isolation(state.matrix, Red, Green))}, Concentration Greens : ${"%.3f".format(delta(state.matrix, Green, Red))}")

    for { (position @ (i, j), c) <- state.cells } {
      def agents = Color.all.map(_.cellColor.get(c)).mkString(",")
      def unsatisfied = Color.all.map { color => simulation.unsatisfied(state, position, color) }.mkString(",")
      output.append(s"""$step,$i,$j,${c.capacity},$agents,$unsatisfied\n""")
    }

  }

}
