package symbolic

import org.junit.Assert.assertEquals
import org.junit.Test

class ScalarTest {

	@Test
	fun getVariables() {
		val a = ScalarC(1.0)
		assertEquals(1.0f, a.eval(), 0.01f)
		assertEquals(1.0f, a.value, 0.01f)
		assertEquals(emptySet<Variable<out Any>>(), a.variables)
	}

	@Test
	fun substitute() {
		assertEquals(ScalarC(2.0), ScalarC(1.0).substitute(ScalarC(1.0), ScalarC(2.0)))
		assertEquals(ScalarC(1.0), ScalarC(1.0).substitute(ScalarC(1.5), ScalarC(2.0)))
	}

}