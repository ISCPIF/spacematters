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
    override def size: Int = 25
    override def greenRatio: Double = 0.5
    override def redRatio: Double = 0.35
    override def maxCapacity: Int = 50
    override def similarWanted: Double = 0.4
  }

  val file1 = new File("/tmp/resultmicro.csv")
  file1.delete()
  val output1 = Resource.fromFile(file1)

  for {
    (state, step) <- simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
    println(s"Step $step: # of unsatisfied: $unsatisfied, Dissimilarity D: ${"%.3f".format(dissimilarity(state, Green, Red))}, Moran I Red: ${"%.3f".format(moran(state, Red))}, Entropy H: ${"%.3f".format(entropy(state, Green, Red))}, Exposure Reds to Greens :${"%.3f".format(exposureOfColor1ToColor2(state, Red, Green))}, Isolation Reds :${"%.3f".format(isolation(state, Red, Green))}, Concentration Greens : ${"%.3f".format(delta(state, Green, Red))}")

    for { (position @ (i, j), c) <- state.cells } {
      def agents = Color.all.map(_.cellColor.get(c)).mkString(",")
      def unsatisfied = Color.all.map { color => simulation.unsatisfied(state, position, color) }.mkString(",")
      output1.append(
        s"""$step,$i,$j,${c.capacity},$agents,$unsatisfied\n""".stripMargin)
    }

  }

  val file2 = new File("/tmp/resultmacro.csv")
  file2.delete()
  val output2 = Resource.fromFile(file2)

  for {
    (state, step) <- simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
    println(s"Step $step")

    output2.append(
      s"""$step,$unsatisfied,${dissimilarity(state, Green, Red)}, ${moran(state, Red)}, ${entropy(state, Green, Red)}, ${exposureOfColor1ToColor2(state, Red, Green)},${exposureOfColor1ToColor2(state, Green, Red)}, ${isolation(state, Red, Green)}, ${isolation(state, Green, Red)},${delta(state, Red, Green)},${delta(state, Green, Red)}\n""".stripMargin)

  }

}
