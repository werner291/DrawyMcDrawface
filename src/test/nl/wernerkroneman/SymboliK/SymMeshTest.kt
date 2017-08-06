package nl.wernerkroneman.SymboliK

import org.junit.Assert.assertEquals
import org.junit.Test
import rendering.Scalar


class SymMeshTest {

	val foo = Variable<Scalar>("foo")
	val bar = Variable<Scalar>("bar")
	val baz = Variable<Scalar>("baz")

	val symMesh = SymMesh(symListOf(CSymFace(
			CSymVector3(ScalarC(0.0), ScalarC(0.0), ScalarC(0.0)),
			CSymVector3(ScalarC(0.0), ScalarC(1.0), ScalarC(0.0)),
			CSymVector3(ScalarC(3.0), ScalarC(9.0), foo))))

	val symMesh2 = SymMesh(symListOf(CSymFace(
			CSymVector3(baz, ScalarC(0.0) + foo, ScalarC(0.0)),
			CSymVector3(ScalarC(0.0), ScalarC(1.0), ScalarC(0.0)),
			CSymVector3(foo + bar, ScalarC(9.0), foo))))

	@Test
	fun eval() {

		val mesh = symMesh.substitute(Variable<Scalar>("foo"), ScalarC(9.0)).eval()

		assertEquals(0.0f, mesh.vertices[0].x, 0.01f)
		assertEquals(0.0f, mesh.vertices[0].y, 0.01f)
		assertEquals(0.0f, mesh.vertices[0].z, 0.01f)

		assertEquals(0.0f, mesh.vertices[1].x, 0.01f)
		assertEquals(1.0f, mesh.vertices[1].y, 0.01f)
		assertEquals(0.0f, mesh.vertices[1].z, 0.01f)

		assertEquals(3.0f, mesh.vertices[2].x, 0.01f)
		assertEquals(9.0f, mesh.vertices[2].y, 0.01f)
		assertEquals(9.0f, mesh.vertices[2].z, 0.01f)

	}

	@Test
	fun getVariables() {

		assertEquals(setOf(foo, bar, baz), symMesh2.variables)
	}

	@Test
	fun substituteInside() {

		val substituted = symMesh2.substitute(foo + bar, foo)

		assertEquals(setOf(foo, baz), substituted.variables)

	}

}