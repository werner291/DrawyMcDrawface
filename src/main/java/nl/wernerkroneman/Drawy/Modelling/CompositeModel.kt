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

/**
 * The CompositeModel represents everything that DrawyMcDrawface
 * knows about the scene described so far.
 *
 *
 * It describes the scene on a very high level, and deals
 * in constraints rather than realisations of those constraints.
 */
abstract class CompositeModel(name: String = "Anonymous composite") : Model(name) {

    abstract val components: MutableSet<Model>
}

class CompositeModelBase(name: String = "Anonymous composite",
                         override val components: MutableSet<Model> = HashSet<Model>()) : CompositeModel() {
    override fun derive(name: String): Model {
        return DerivedCompositeModel(this)
    }
}

class DerivedCompositeModel(val base: CompositeModel) : CompositeModel() {

    override fun derive(name: String): Model {
        return DerivedCompositeModel(this)
    }

    override val components = MutableRelativeSet(base.components)
}