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

import nl.wernerkroneman.Drawy.Interface.BlockingInteractor
import nl.wernerkroneman.Drawy.Modelling.ModelSpecification

/**
 * Resolver that starts a description session.
 */
class RecursiveSessionResolver(private val interactor: BlockingInteractor) : ModelResolver {
    internal var sessionContext: DescriptionSession? = null

    fun setSessionContext(sessionContext: DescriptionSession) {
        this.sessionContext = sessionContext
    }

    /**
     * Resolve an object by starting a recursive session.
     *
     *
     * Use setSessionContext first.

     * @param name Name of the object.
     * *
     * @return The object
     * *
     * @pre sessionContext != null
     */
    override fun resolveObject(name: String): ModelSpecification {

        if (sessionContext == null) {
            throw IllegalStateException("Session context not initialized.")
        }

        if (interactor.askUserYesNo("I don't know what '$name' is. Can you tell me?")) {

            interactor.tellUser("Ok, please tell me about " + name)

            val model = sessionContext!!.runSession()

            return model

        } else {
            interactor.tellUser("Ok... I guess?")
            throw RuntimeException("Uncooperative user.")
        }
    }
}
