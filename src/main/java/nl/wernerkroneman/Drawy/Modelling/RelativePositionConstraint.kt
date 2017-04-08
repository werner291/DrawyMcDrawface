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

package nl.wernerkroneman.Drawy.Modelling

import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint.RelativePosition.DimensionOrder.*

class RelativePositionConstraint(var a: RelativeConstraintContext.Positionable,
                                 var b: RelativeConstraintContext.Positionable,
                                 var pos: RelativePosition,
                                 var dist: Distance) : Constraint {

    override fun toString(): String {
        return "RelativePositionConstraint{" +
                ", b=" + b +
                ", pos=" + pos +
                '}'
    }

    class RelativePosition(xRel: RelativePosition.DimensionOrder,
                           yRel: RelativePosition.DimensionOrder,
                           zRel: RelativePosition.DimensionOrder) {

        enum class DimensionOrder {
            BEFORE, AFTER, SAME
        }

        var rel: Array<DimensionOrder>

        init {
            this.rel = arrayOf(xRel, yRel, zRel)
        }

    }

    companion object {

        var ABOVE = RelativePosition(SAME, AFTER, SAME)
        var BELOW = RelativePosition(SAME, BEFORE, SAME)
        var FRONT = RelativePosition(SAME, SAME, AFTER)
        var BEHIND = RelativePosition(SAME, SAME, BEFORE)
        var LEFT = RelativePosition(BEFORE, SAME, SAME)
        var RIGHT = RelativePosition(AFTER, SAME, SAME)
    }
}
