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

class AnyModel(name: String) : Model(name) {

    internal var options: MutableList<Model> = ArrayList()

    override fun <V : Any> accept(visitor: ModelVisitor<V>): V {
        return visitor.visit(this)
    }

    val any: Model
        get() = options[random.nextInt(options.size)]

    override fun toString(): String {
        return "AnyModel{" +
                "options=" + options +
                '}'
    }

    fun addOption(newSubjectDef: Model) {
        options.add(newSubjectDef)
    }

    fun getOptions(): Collection<Model> {
        return options
    }

    companion object {

        internal var random = Random()
    }
}
