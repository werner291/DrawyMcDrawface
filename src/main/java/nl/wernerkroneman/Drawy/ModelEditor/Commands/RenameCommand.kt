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

import nl.wernerkroneman.Drawy.Modelling.ModelSpecification

class RenameCommand(previous: EditorCommand?,
        // TODO implement and use deep referencing.
                    val target: ModelSpecification,
                    val newName: String) : EditorCommand(false, previous) {

    var oldName = target.name

    override fun onApply() {
        oldName = target.name
        target.name = newName
    }

    override fun onRevert() {
        target.name = oldName
    }

    override fun toString(): String {
        return "RenameCommand(target=$target, newName='$newName', oldName='$oldName') : " + super.toString()
    }
}
