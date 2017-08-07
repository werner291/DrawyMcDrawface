package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Test

class IterableTest {

	val x = VarScalar("x")
	val y = VarScalar("y")
	val z = VarScalar("z")

	@Test
	fun sizeTest() {

		assertEquals(5, CSymIterable(x, x, x, z, y).size.eval())

	}

	@Test
	fun mapTest() {

		assertEquals(CSymIterable(y, y, y, y), CSymIterable(x, x, x, x).map(y).simplifyFully())
		assertEquals(CSymIterable(x + ScalarC(1),
								  x + ScalarC(1),
								  y + ScalarC(1),
								  z + ScalarC(1)),
					 CSymIterable(x, x, y, z).map(x + ScalarC(1), x).simplifyFully())


	}


}