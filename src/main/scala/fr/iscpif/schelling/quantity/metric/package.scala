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
package fr.iscpif.schelling.quantity

import math._

package object metric {

  def ratio(color: Color, cells: Seq[Cell]) =
    total(color, cells).toDouble / cells.map(_.population).sum

  def total(color: Color, cells: Seq[Cell]) =
    cells.map(color.cellColor.get).sum

  def dissimilarity(cells: Seq[Seq[Cell]], color1: Color, color2: Color): Double = {
    val flatCells = cells.flatten
    val totalPopulation = Seq(color1, color2).map{ color => color -> total(color, flatCells) }.toMap

    flatCells.map {
      cell =>
        val nbColor1 = color1.cellColor.get(cell)
        val nbColor2 = color2.cellColor.get(cell)
        0.5 * abs(nbColor1.toDouble / totalPopulation(color1) - nbColor2.toDouble / totalPopulation(color2))
    }.sum
  }


}
