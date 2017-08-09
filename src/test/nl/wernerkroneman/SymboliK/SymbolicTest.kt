package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test


class SymbolicTest {

	@Test
	fun simpleVarTest() {

		val x = Variable<Int>("foo")

		assertTrue(x.variables.first() == x)

		assertEquals(ScalarC(42.0f), x.substitute(x, ScalarC(42.0f)))

	}

}
