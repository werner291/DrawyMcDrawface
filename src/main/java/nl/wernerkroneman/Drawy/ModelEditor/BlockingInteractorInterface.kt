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
 * Created by werner on 1-1-17.
 */
interface BlockingInteractorInterface {
    /**
     * Present a question to the user and block until a response is provided.
     *
     *
     * *DO NOT CALL FROM EVENT DISPATCH THREAD AS THIS MAY CAUSE A DEADLOCK!*

     * @param question The question to ask
     * *
     * @return The response.
     */
    fun askUserString(question: String): String

    fun askUserYesNo(question: String): Boolean

    fun tellUser(toTell: String)
}
