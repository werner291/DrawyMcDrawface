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

import java.util.*

interface Location

val NOT_INTERSECTING_SOLID = object : Location {}

class UnionLocation(val unionOf: MutableSet<Location> = HashSet<Location>())

class IntersectionLocation(val intersectionOf: MutableSet<Location> = HashSet<Location>()) : Location

class RelativeLocation(val right: ModelSpecification,
                       val relPos: RelativePosition,
                       val dist: Distance) : Location

fun combineLocations(location: Location?, location1: Location?): Location? {
    return when {
        location == null -> location1
        location1 == null -> location
        else -> IntersectionLocation(mutableSetOf(location, location1))
    }
}