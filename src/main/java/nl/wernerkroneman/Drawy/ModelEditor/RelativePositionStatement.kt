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

package nl.wernerkroneman.Drawy.ModelEditor

import nl.wernerkroneman.Drawy.Modelling.Distance
import nl.wernerkroneman.Drawy.Modelling.RelativeConstraintContext
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint

import java.util.function.Supplier

/**
 * Represents a statement that creates a constraint
 * between two components in a CompositeModel.
 */
class RelativePositionStatement(val a: Supplier<out RelativeConstraintContext.Positionable>,
                                val b: Supplier<out RelativeConstraintContext.Positionable>,
                                val pos: RelativePositionConstraint.RelativePosition,
                                private val target: Supplier<out RelativeConstraintContext>) : EditorCommand() {

    internal override fun onApply() {
        target.get().constraints.add(RelativePositionConstraint(a.get(),
                b.get(),
                pos,
                Distance.ANY))
    }

    override fun toString(): String {
        return "RelativePositionStatement($a $pos $b)"
    }

}
