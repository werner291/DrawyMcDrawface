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
import nl.wernerkroneman.Drawy.Modelling.Distance
import nl.wernerkroneman.Drawy.Modelling.RelativePositionConstraint
import org.junit.Test

class KnowledgeJSONSerializerTest {

    @Test
    fun testSimple() {

        val knowledge = Knowledge.knowledgeWithPrimitives()

        val composite = CompositeModel("cube above sphere")

        val cube = composite.addComponentForModel(
                knowledge.getObject("Cube")!!)
        val sphere = composite.addComponentForModel(
                knowledge.getObject("Sphere")!!)
        composite.addConstraint(RelativePositionConstraint(cube, sphere, RelativePositionConstraint.ABOVE, Distance.ANY))

        val serializer = KnowledgeJSONSerializer(knowledge)

        val jsonObject = serializer.serializeKnowledge()

        println(jsonObject.toJSONString())

        val reconstructed = KnowledgeJSONDeserializer.deserializeJSON(jsonObject)

        //Assert.assertTrue(KnowledgeIntegrityChecker.checkIntegrity(reconstructed));

    }
}
