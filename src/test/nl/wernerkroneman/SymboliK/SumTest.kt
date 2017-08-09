package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Test

class SumTest {

	@Test
	fun getters() {
		assertEquals(ScalarC(1.0f), (ScalarC(1.0f) + ScalarC(3.0f)).terms[0])
		assertEquals(ScalarC(3.0f), (ScalarC(1.0f) + ScalarC(3.0f)).terms[1])
	}

	@Test
	fun testSubstitute() {

		val a = (ScalarC(1.0) + 3.5) + Variable<Scalar>("foo")

		assertEquals(ScalarC(9), a.substitute(Variable<Scalar>("foo"), ScalarC(4.5)).simplifyFully())

		val b = Sum(Sum(ScalarC(1.0), ScalarC(3.5)), ScalarC(4.5))

		assertEquals(
				ScalarC(4.5),
				b.substitute<Number>(Sum(ScalarC(1.0), ScalarC(3.5)), ScalarC(0.0)).simplifyFully()
		)

	}

	@Test
	fun testEval() {
		assertEquals(ScalarC(5), (ScalarC(1.0) + ScalarC(3.0) + ScalarC(1.0)).simplifyFully())
	}

	@Test
	fun testGetVariables() {
		val a = (ScalarC(1.0) + ScalarC(3.5)) + Variable<Scalar>("foo")

		assertEquals(setOf(Variable<Number>("foo")), a.variables)

		val b = ScalarC(1.0) + ScalarC(3.5) + ScalarC(9.6)

		assertEquals(emptySet<Variable<out Any>>(), b.variables)
	}

	@Test
	fun testFlatten() {

		val x = Variable<Scalar>("x")
		val y = Variable<Scalar>("y")
		val z = Variable<Scalar>("z")

		assertEquals(x + x + y + z + y, x + ((x + y) + z + y))

	}

	@Test
	fun testSimplify() {

		val x = VarScalar("x")

		assertEquals(ScalarC(3) * x, (x + x + x).simplifyFully())
		assertEquals(ScalarC(9.0f), (x - x + 9.0f).simplifyFully())
		assertEquals(x + 1.0f, (x + 1.0f).simplifyFully())

	}
}
