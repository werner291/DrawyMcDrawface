package nl.wernerkroneman.SymboliK

import nl.wernerkroneman.DrawyMcDrawface.axisAlignedUnitCube
import org.junit.Assert.assertEquals
import org.junit.Test

class PrimitivesTest {

	@Test
	fun cubeTestAxisAligned() {

		// An AA cube has 3 degrees of freedom
		assertEquals(3, axisAlignedUnitCube.variables.size)

		assertEquals(IntC(6), axisAlignedUnitCube.faces.size.simplifyFully())

		assertEquals(IntC(8), axisAlignedUnitCube.vertices.size.simplifyFully())



		//TODOassertTrue(axisAlignedUnitCube.faces.all {it.edges.size == 4 &&
		//		it.vertices.size == 4})

	}


}

