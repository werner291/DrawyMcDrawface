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
 * Scaffolding classes to make the transition from the Parsey-output
 * to the abstract modelling easier.
 */
sealed class SceneComponent(val relations: Set<SceneComponentRelation>) {

    class CompositeComponentReference(val component: CompositeModel.Component,
                                      val context: CompositeModel,
                                      relations: Set<SceneComponentRelation>) : SceneComponent(relations)

    class NewComponent(val model: Model,
                       relations: Set<SceneComponentRelation>) : SceneComponent(relations)
}