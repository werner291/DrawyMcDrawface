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

package nl.wernerkroneman.Drawy.ConcreteModelling

class Color(val red: Float,
            val green: Float,
            val blue: Float,
            val alpha: Float = 1.0f)

val BLACK = Color(0f, 0f, 0f)
val RED = Color(1f, 0f, 0f)
val GREEN = Color(0f, 1f, 0f)
val YELLOW = Color(1f, 1f, 0f)
val BLUE = Color(0f, 0f, 1f)

val LIGHT_GRAY = Color(0.7f, 0.7f, 0.7f)

class Material(val ambient: Color,
               val diffuse: Color,
               val emissive: Color,
               val specular: Color)

val DEFAULT_GRAY = Material(LIGHT_GRAY,
        LIGHT_GRAY,
        BLACK,
        BLACK)
