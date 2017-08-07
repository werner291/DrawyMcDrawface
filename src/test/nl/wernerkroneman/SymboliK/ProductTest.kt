package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Test

val x = VarScalar("x")
val y = VarScalar("y")
val z = VarScalar("z")

class ProductTest {
	@Test
	fun substituteInside() {

	}

	@Test
	fun eval() {

	}

	@Test
	fun getVariables() {

	}

	@Test
	fun getters() {


	}

	@Test
	fun combineCoefficients() {

		assertEquals(135.0f, (3.0f * x * 9.0f * 5.0f * y)
				.flatten()
				.combineCoefficients()
				.factors[0].eval(), 0.01f)

	}

	@Test
	fun likeFactorsToExponents() {

		assertEquals(Power(x, ScalarC(3)), (x * x * x).simplifyFully())

	}

}