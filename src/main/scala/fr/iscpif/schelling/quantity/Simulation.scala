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
    override def size: Int = 100
    override def greenRatio: Double = 0.65
    override def redRatio: Double = 0.25
    override def maxCapacity: Int = 10
    override def similarWanted: Double = 0.45
  }

  val file = new File("/tmp/result.csv")
  file.delete()

  val output = Resource.fromFile(file)

  for {
    (state, step) <- simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
    println(s"Step $step: Number of unsatisfied: $unsatisfied, dissimilarity index D: ${"%.3f".format(dissimilarity(state.matrix, Green, Red))}, entropy index H: ${"%.3f".format(entropy(state.matrix, Green, Red))}, Exposure of Reds to Greens :${"%.3f".format(exposureOfColor1ToColor2(state.matrix, Red, Green))}, Isolation index of Reds :${"%.3f".format(isolation(state.matrix, Red, Green))}")

    for { ((i, j), c) <- state.cells }
      output.append(s"""$i,$j,${c.capacity},${Color.all.map(_.cellColor.get(c)).mkString(",")}\n""")

  }

}
