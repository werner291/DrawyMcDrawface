package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test


class SymbolicTest {

	@Test
	fun simpleVarTest() {

		val x = Variable<Int>("foo")

		assertTrue(x.variables.first() == x)

		assertEquals(42.0f, x.substitute(x, ScalarC(42.0f)).eval())

	}

	@Test(expected = RuntimeException::class)
	fun badEvalTest() {

		val x = Variable<Int>("foo")

		x.eval()
	}

}
