package nl.wernerkroneman.SymboliK

import nl.wernerkroneman.DrawyMcDrawface.axisAlignedUnitCube
import nl.wernerkroneman.DrawyMcDrawface.faces
import nl.wernerkroneman.DrawyMcDrawface.vertices
import org.junit.Assert.assertEquals
import org.junit.Test

class PrimitivesTest {

	@Test
	fun cubeTestAxisAligned() {

		// An AA cube has 3 degrees of freedom
		assertEquals(3, axisAlignedUnitCube.variables.size)

		assertEquals(6, axisAlignedUnitCube.faces.size.eval())

		assertEquals(8, axisAlignedUnitCube.vertices.size.eval())



		//TODOassertTrue(axisAlignedUnitCube.faces.all {it.edges.size == 4 &&
		//		it.vertices.size == 4})

	}


}

