/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.iscpif.spacematters.initialise

import fr.iscpif.mgo._
import fr.iscpif.mgo.mutation._
import fr.iscpif.mgo.crossover._
import fr.iscpif.spacematters.model._
import metric._
import Moran._
import monocle.Lens
import monocle.macros.GenLens

import scala.util.Random

object Initialise extends App {

  val size = 20
  val avgCapacity = 100

  case class World(size: Int, matrix: Matrix[Int], mutation: Option[Int] = None)

  trait PSE <: NoFitness
      with HitMapArchive
      with GeneticBreeding
      with BinaryTournamentSelection
      with DynamicMutation
      with TournamentOnHitCount
      with HierarchicalRanking
      with RandomNicheElitism
      with CounterTermination
      with ProportionalNumberOfRound
      with PhenotypeGridNiche {
    type G = World
  }

  def flatWorld(size: Int, capacity: Int) = World(size, Matrix(Seq.fill(size, size)(capacity)))

  def mutateWorld(world: World, sigma: Double, moves: Int)(implicit rng: Random) = {
    val buffer = world.matrix.map(_.toArray)

    def draw = rng.nextInt(world.size)

    def oneMove = {
      val (fromI, fromJ) = (draw, draw)
      val (toI, toJ) = (draw, draw)
      val fromCap = buffer(fromI)(fromJ)
      val toCap = buffer(toI)(toJ)
      val quantity = {
        val q = math.abs((rng.nextGaussian() * sigma).toInt)
        val bounded = math.min(fromCap, q)
        if (bounded == 0) 0 else rng.nextInt(bounded)
      }

      buffer(toI)(toJ) = toCap + quantity
      buffer(fromI)(fromJ) = fromCap - quantity
    }

    for (m ← 0 until moves) oneMove
    world.copy(matrix = Matrix(buffer.map(_.toSeq)))
  }

  def evaluateMatrix(matrix: Matrix[Int]): Seq[Double] = {
    def id = (x: Int) ⇒ x.toDouble

    def trash = Seq.fill(4)(Double.NegativeInfinity)

    val (s, r2) = slope(matrix, id)
    val dm = distanceMean(matrix, id)
    val cm = capacityMoran(matrix, id)
    val e = entropy(matrix, id)

    if (s < -4 || r2 < 0.5 || e < 0.5) trash
    else Seq(s, cm, dm, e)
  }

  val pse = new PSE {
    def parameterableMutation(proportion: Double, cellRatio: Double) =
      (g: G, p: Population[G, P, F], a: A, rng: Random) ⇒ mutateWorld(g, avgCapacity / proportion, (g.size / cellRatio).toInt)(rng)

    override def fromMutation: Lens[World, Option[Int]] = GenLens[World](_.mutation)
    override def mutations: Vector[Mutation] =
      Vector(
        parameterableMutation(2.0, 10),
        parameterableMutation(5.0, 25),
        parameterableMutation(10.0, 50),
        parameterableMutation(20.0, 100)
      )

    override def crossovers: Vector[Crossover] = Vector.empty

    override def steps: Int = 100
    override def lambda: Int = 200

    override def express(g: World, rng: Random): Seq[Double] = evaluateMatrix(g.matrix)
    override def gridSize: Seq[Double] = Seq(0.01, 0.01, 0.01, 0.01)

    override def randomGenome(implicit rng: Random): World =
      mutateWorld(
        flatWorld(size, avgCapacity),
        rng.nextDouble() * avgCapacity / 2,
        rng.nextInt(size * size) / 2)(rng)
  }

  implicit val rng = new Random(42)
  val res = pse.evolve.untilConverged {
    s ⇒
      println(s.terminationState)
      for {
        (w, is) ← s.population.map(i ⇒ i.genome -> i.phenotype)
      } {
        println(is)
      }
  }

}
