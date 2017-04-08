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
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint

/**
 * Command that causes the creation of a component
 * in a CompositeModel.
 * A supplier that supplies the CompositeModel on which to execute this command.
 */
class CreateEntityEditorCommand(
        internal var target: () -> CompositeModel,
        var what: SceneComponent.NewComponent? = null,
        var created: CompositeModel.Component? = null,
        previous: EditorCommand?) : EditorCommand(previous = previous) {

    override fun toString(): String {
        return "Create $what in $target"
    }

    override fun onApply() {
        val scene = target()

        addAllComponents(scene, what!!)
    }

    fun addAllComponents(scene: CompositeModel,
                         scaffold: SceneComponent,
                         scaffoldToComponent: MutableMap<SceneComponent,
                                 CompositeModel.Component> = mutableMapOf()) {

        if (scaffold is SceneComponent.NewComponent &&
                scaffold !in scaffoldToComponent.keys) {
            val component = CompositeModel.Component(scaffold.model)
            scaffoldToComponent[scaffold] = component
            scene.components.add(component)

            created = component // TODO improve on composite objects
        }

        if (scaffold is SceneComponent.CompositeComponentReference) {
            if (scaffold.context != scene) {
                throw UnsupportedOperationException("Deep referencing not supported yet.")
            } else {
                scaffoldToComponent[scaffold] = scaffold.component
            }
        }

        for (rel in scaffold.relations) {
            addAllComponents(scene, rel.right, scaffoldToComponent)
        }

        // Do this last to ensure all depensent components have been created.
        for (rel in scaffold.relations) {

            scene.constraints.add(
                    RelativePositionConstraint(scaffoldToComponent[scaffold]!!,
                            scaffoldToComponent[rel.right]!!,
                            rel.relPos,
                            rel.dist))
        }
    }

    override fun onRevert() {
        target().components.remove(created)

        created = null
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