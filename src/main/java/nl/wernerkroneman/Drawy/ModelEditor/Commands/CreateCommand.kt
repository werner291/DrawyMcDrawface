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

package nl.wernerkroneman.Drawy.ModelEditor.Commands

import nl.wernerkroneman.Drawy.Modelling.CompositeModelSpecification
import nl.wernerkroneman.Drawy.Modelling.ModelSpecification

/**
 * Command that causes the creation of a component
 * in a CompositeModelSpecification.
 * A supplier that supplies the CompositeModelSpecification on which to execute this command.
 */
class CreateCommand(
        internal var target: () -> CompositeModelSpecification,
        val what: ModelSpecification,
        previous: EditorCommand?) : EditorCommand(previous = previous) {

    override fun toString(): String {
        return "Create $what in $target"
    }

    override fun onApply() {
        val scene = target()

        if (what in scene.directComponents) {
            throw IllegalStateException("Component already in target.")
        }

        scene.directComponents.add(what)
    }

    override fun onRevert() {
        target().directComponents.remove(what)
    }

    /**
     * Return a supplier that supplies the resulting Component.
     *
     *
     * The supplier only be executed after this function is applied,
     * throws otherwise.

     * @return the supplier.
     */
    /*val resultSupplier: Supplier<CompositeModelSpecification.Component>
        get() = object : Supplier<CompositeModelSpecification.Component> {
            override fun get(): CompositeModelSpecification.Component {
                if (!applied) {
                    throw IllegalStateException("Statement not applied")
                }
                return created
            }

            override fun toString(): String {
                return "(Created by " + this@CreateCommand + ")"
            }
        }*/
}