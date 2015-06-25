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

import fr.iscpif.spacematters.model._
import fr.iscpif.spacematters.model.metric._
import Moran._

object IndicatorTest extends App {
  def id = (x: Int) ⇒ x.toDouble

  def capacityMoran(matrix: Matrix[Int]) =
    moran(
      matrix,
      id,
      distanceDecayNeighbourhoodPairs[Int](1 / distance(_, _))
    )

  val flatMatrix = Matrix(Seq.fill(4, 4) { 10 })

  val matrix = Matrix(Seq.tabulate(4, 4) { (i, j) ⇒ math.exp(4 * math.log(i + 1)).toInt })
  println(slope(matrix, id))
  println(distanceMean(flatMatrix, id))
  println(capacityMoran(matrix))

}
