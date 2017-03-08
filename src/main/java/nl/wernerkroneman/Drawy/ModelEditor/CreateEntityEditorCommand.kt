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

import nl.wernerkroneman.Drawy.Modelling.CompositeModel
import nl.wernerkroneman.Drawy.Modelling.Model

/**
 * Command that causes the creation of a component
 * in a CompositeModel.
 * A supplier that supplies the CompositeModel on which to execute this command.
 */
class CreateEntityEditorCommand internal constructor(
        internal var target: () -> CompositeModel?,
        var what: Model? = null,
        var created: CompositeModel.Component? = null) : EditorCommand() {

    override fun toString(): String {
        return "Create $what in $target"
    }

    internal override fun onApply() {
        created = target()!!.addComponentForModel(what ?:
                throw NullPointerException("Expression 'what' must not be null"))
    }

    internal override fun onRevert() {
        target()!!.components.remove(created)
    }

    /**
     * Return a supplier that supplies the resulting Component.
     *
     *
     * The supplier only be executed after this function is applied,
     * throws otherwise.

     * @return the supplier.
     */
    /*val resultSupplier: Supplier<CompositeModel.Component>
        get() = object : Supplier<CompositeModel.Component> {
            override fun get(): CompositeModel.Component {
                if (!applied) {
                    throw IllegalStateException("Statement not applied")
                }
                return created
            }

            override fun toString(): String {
                return "(Created by " + this@CreateEntityEditorCommand + ")"
            }
        }*/
}
