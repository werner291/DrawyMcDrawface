package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Test

class PrimitivesTest {

	@Test
	fun cubeTestAxisAligned() {

		// An AA cube has 3 degrees of freedom
		assertEquals(3, axisAlignedUnitCube.variables.size)

		assertEquals(8, axisAlignedUnitCube.vertices.size.eval())

		assertEquals(12, axisAlignedUnitCube.edges.size)

		assertEquals(6, axisAlignedUnitCube.faces.size)

		//TODOassertTrue(axisAlignedUnitCube.faces.all {it.edges.size == 4 &&
		//		it.vertices.size == 4})

	}


}

