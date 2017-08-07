package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * Created by werner on 7-8-17.
 */
class GetterTest {
	@Test
	fun simplify() {

		val x = VarScalar("x")
		val y = VarScalar("y")
		val z = VarScalar("z")

		assertEquals(x, CSymVector3(x, y, z).x.simplifyFully())
		assertEquals(y, CSymVector3(x, y, z).y.simplifyFully())
		assertEquals(z, CSymVector3(x, y, z).z.simplifyFully())

	}

}