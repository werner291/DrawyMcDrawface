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

package nl.wernerkroneman.Drawy.Modelling;

class VariantModel(name: String,
                   val base: Model) : Model(name) {

    val modifiers : MutableSet<Modifier> = mutableSetOf()

    override fun <V : Any> accept(visitor: ModelVisitor<V>): V {
        return visitor.visit(this)
    }
}

abstract class Modifier
abstract class SizeModifier : Modifier()

class RelativeSize(val relativeSize: Double) : SizeModifier()

val BIG = RelativeSize(1.5)
val SMALL = RelativeSize(1/1.5)