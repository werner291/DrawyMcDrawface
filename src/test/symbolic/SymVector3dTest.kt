package symbolic

import org.junit.Assert.assertEquals
import org.junit.Test
import rendering.Scalar

/**
 * Created by werner on 3-8-17.
 */
class SymVector3dTest {
	@Test
	fun eval() {

		val a = CSymVector3(ScalarC(1.0), ScalarC(2.0), ScalarC(1.5))

		val b = a.eval()

		assertEquals(1.0f, b.x, 0.01f)
		assertEquals(2.0f, b.y, 0.01f)
		assertEquals(1.5f, b.z, 0.01f)

	}

	@Test
	fun substitute() {

		val a = CSymVector3(ScalarC(1.0), ScalarC(2.0), ScalarC(1.5) + VarScalar("x"))

		assertEquals(VarScalar("y"), (a.substitute(ScalarC(1.0), VarScalar("y")) as CSymVector3).x)
		assertEquals(VarScalar("y"), a.substituteInside(ScalarC(1.0), VarScalar("y")).x)

	}

	@Test
	fun getVariables() {
		assertEquals(setOf(Variable<Scalar>("y"), Variable<Scalar>("foo")),
					 CSymVector3(Variable<Scalar>("y"), ScalarC(2.0), ScalarC(1.5) + Variable<Scalar>("foo")).variables)
	}

	@Test
	fun vectorSumTest() {

		val x = Variable<Scalar>("foo")
		val y = Variable<Scalar>("y")

		val opA = CSymVector3(y, y, ScalarC(1.5))
		val opB = CSymVector3(x, ScalarC(0.5), ScalarC(-10.0))

		val result = opA + opB

		assertEquals(VectorSum(opA, opB), result)

		val evalResult = result.substitute(x, ScalarC(0.5))
				.substitute(y, ScalarC(2.0)).eval()

		assertEquals(2.5f, evalResult.x, 0.01f)
		assertEquals(2.5f, evalResult.y, 0.01f)
		assertEquals(-8.5f, evalResult.z, 0.01f)

	}

}


