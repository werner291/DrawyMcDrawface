/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy.AbstractToConcreteConverter

import nl.wernerkroneman.Drawy.Algorithms.topologicalSort
import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint

fun CompositeModel.componentsInTopoligicalOrder(): List<CompositeModel.Component> {

    val constraints = this.constraints

    return topologicalSort<CompositeModel.Component>(components, { compA ->
        constraints.filter { it is RelativePositionConstraint }
                .filter { (it as RelativePositionConstraint).a == compA }
                .map { (it as RelativePositionConstraint).b as CompositeModel.Component }
    })

}