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

/**
 * An action to be applied.

 * It follows a Command design pattern.

 * Most commands are revertible.
 */
abstract class EditorCommand {

    // Whether this command has been applied or not.
    internal var applied = false

    fun apply() {
        if (applied) {
            throw RuntimeException("Trying to apply a statement that was already applied!")
        }

        onApply()

        applied = true
    }

    fun revert() {
        if (!applied) {
            throw RuntimeException("Trying to revert a non-applied statement.")
        }
        applied = false
        onRevert()
    }

    open internal abstract fun onApply()

    open internal fun onRevert() {
        throw UnsupportedOperationException("This command cannot be reverted.")
    }
}

