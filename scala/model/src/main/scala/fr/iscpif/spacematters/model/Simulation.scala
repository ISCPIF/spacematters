package fr.iscpif.spacematters.model

import java.io.File

import initial._
import move._
import metric._
import stop._
import Moran._

import scalax.io.Resource

import scala.util.Random

object Simulation extends App {

  implicit val rng = new Random

  val simulation = new Schelling with RandomState with RandomMoves with SpeilmanStop {
    override def greenRatio: Double = 0.05
    override def redRatio: Double = 0.05
    override def similarWanted: Double = 0.6

    override def capacityGrid(implicit rng: Random): Seq[Seq[Int]] =
      initial.normalize(initial.readMatrix(new File(args(0)), 20), 100000)
  }

  println(simulation.run)

  /* val dir = "/tmp/"

  val file1 = new File(dir + "resultmicro.csv")
  file1.delete()
  val output1 = Resource.fromFile(file1)

  val file2 = new File(dir + "resultmacro.csv")
  file2.delete()
  val output2 = Resource.fromFile(file2)

  for {
    (state, step) ← simulation.states.take(100).zipWithIndex
  } {
    def unsatisfied = simulation.unsatisfieds(state).map(_.number).sum
    println(s"Step $step: # of unsatisfied: $unsatisfied, Dissimilarity D: ${"%.3f".format(dissimilarity(state, Green, Red))}, Moran I Red: ${"%.3f".format(colorRatioMoran(state, Red))}, Entropy H: ${"%.3f".format(segregationEntropy(state, Green, Red))}, Exposure Reds to Greens :${"%.3f".format(exposureOfColor1ToColor2(state, Red, Green))}, Isolation Reds :${"%.3f".format(isolation(state, Red, Green))}, Concentration Greens : ${"%.3f".format(delta(state, Green, Red))}")

    for { (position @ (i, j), c) ← state.cells } {
      def agents = Color.all.map(_.cellColor.get(c)).mkString(",")
      def unsatisfied = Color.all.map { color ⇒ simulation.unsatisfied(state, position, color) }.mkString(",")
      output1.append(
        s"""$step,$i,$j,${c.capacity},$agents,$unsatisfied\n""".stripMargin)
    }

    val size = simulation.size
    val greenRatio = simulation.greenRatio
    val redRatio = simulation.redRatio
    val similarWanted = simulation.similarWanted

    output2.append(
      s"""$step, $unsatisfied,${dissimilarity(state, Green, Red)}, ${colorRatioMoran(state, Red)}, ${segregationEntropy(state, Green, Red)}, ${exposureOfColor1ToColor2(state, Red, Green)},${exposureOfColor1ToColor2(state, Green, Red)}, ${isolation(state, Red, Green)}, ${isolation(state, Green, Red)},${delta(state, Red, Green)},${delta(state, Green, Red)}, $size, $greenRatio,$redRatio, $similarWanted\n""".stripMargin)

  }*/

}
