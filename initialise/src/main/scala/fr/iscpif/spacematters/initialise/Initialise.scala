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
import monocle.Lens
import monocle.macros.GenLens

import scala.util.Random

object Initialise {

  val size = 50
  val avgCapacity = 100

  case class World(size: Int, matrix: Seq[Seq[Int]], mutation: Option[Int] = None)

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

  def flatWorld(size: Int, capacity: Int) = World(size, Seq.fill(size, size)(capacity))

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
        rng.nextInt(math.min(fromCap, q))
      }

      buffer(toI)(toJ) = toCap + quantity
      buffer(fromI)(fromJ) = fromCap - quantity
    }

    for (m ← 0 until moves) oneMove
    world.copy(matrix = buffer.map(_.toSeq))
  }

  val pse = new PSE {
    def parameterableMutation(proportion: Double, cellRatio: Double) =
      (g: G, p: Population[G, P, F], a: A, rng: Random) ⇒ mutateWorld(g, avgCapacity / proportion, (g.size / cellRatio).toInt)(rng)

    override def fromMutation: Lens[World, Option[Int]] = GenLens[World](_.mutation)
    override def mutations: Vector[Mutation] =
      Vector(
        parameterableMutation(2.0, 10),
        parameterableMutation(4.0, 25),
        parameterableMutation(8.0, 50),
        parameterableMutation(16.0, 100)
      )

    override def crossovers: Vector[Crossover] = Vector.empty

    override def steps: Int = 1000
    override def lambda: Int = 1000

    override def express(g: World, rng: Random): Seq[Double] = ???
    override def gridSize: Seq[Double] = ???

    override def randomGenome(implicit rng: Random): World =
      mutateWorld(flatWorld(size, avgCapacity), avgCapacity, size * size)(rng)
  }

}
