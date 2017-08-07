package nl.wernerkroneman.SymboliK

import org.joml.Vector3f


/**
 *
 */
data class CSymVector3(val x: SymScalar,
					   val y: SymScalar,
					   val z: SymScalar) : Symbolic<Vector3> {
	override fun simplify(depth: Int): Symbolic<Vector3> {
		return CSymVector3(x.simplify(depth - 1), y.simplify(depth - 1), z.simplify(depth - 1))
	}

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			CSymVector3(x.substitute(find, replace),
						y.substitute(find, replace),
						z.substitute(find, replace))

	override fun eval() = Vector3f(x.eval(), y.eval(), z.eval())

	override val variables = x.variables + y.variables + z.variables
}

data class VectorSum(val a: Symbolic<Vector3f>, val b: Symbolic<Vector3f>) : Symbolic<Vector3f> {
	override fun simplify(depth: Int): Symbolic<Vector3f> {

		val simpleA = a.simplify(depth - 1)
		val simpleB = b.simplify(depth - 1)

		return if (simpleA is CSymVector3 && simpleB is CSymVector3)
			CSymVector3(simpleA.x + simpleB.x,
						simpleA.y + simpleB.y,
						simpleA.z + simpleB.z).simplify(depth - 1)
		else
			VectorSum(a.simplify(depth - 1), b.simplify(depth - 1))
	}

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			VectorSum(a.substitute(find, replace), b.substitute(find, replace))

	override fun eval() = a.eval().add(b.eval())

	override val variables
		get() = a.variables + b.variables

}

data class VectorDifference(val a: Symbolic<Vector3f>, val b: Symbolic<Vector3f>) : Symbolic<Vector3f> {
	override fun simplify(depth: Int): Symbolic<Vector3f> {
		return VectorDifference(a.simplify(depth - 1), b.simplify(depth - 1))
	}

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			VectorSum(a.substitute(find, replace), b.substitute(find, replace))

	override fun eval() = a.eval().sub(b.eval())

	override val variables
		get() = a.variables + b.variables

}

operator fun Symbolic<Vector3f>.minus(b: Symbolic<Vector3f>) =
		VectorDifference(this, b)

fun Symbolic<Vector3>.length(): SymScalar {
	return Sqrt(this.x * this.x
						+ this.y * this.y
						+ this.z * this.z)
}

private val Symbolic<Vector3f>.x
	get() = getter(CSymVector3::x, this)

private val Symbolic<Vector3f>.y
	get() = getter(CSymVector3::y, this)

private val Symbolic<Vector3f>.z
	get() = getter(CSymVector3::z, this)

operator fun CSymVector3.plus(opB: CSymVector3) = VectorSum(this, opB)

