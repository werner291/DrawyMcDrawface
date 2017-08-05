package symbolic

import org.joml.Vector3f
import rendering.Scalar
import rendering.Vector3

typealias SymScalar = Symbolic<Scalar>

// TODO generalize to non-doubles
data class CSymVector3(val x: SymScalar,
					   val y: SymScalar,
					   val z: SymScalar) : Symbolic<Vector3> {
	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			CSymVector3(x.substitute(find, replace),
						y.substitute(find, replace),
						z.substitute(find, replace))

	override fun eval() = Vector3f(x.eval().toFloat(),
								   y.eval().toFloat(),
								   z.eval().toFloat())

	override val variables = x.variables + y.variables + z.variables
}

data class VectorSum(val a: Symbolic<Vector3f>, val b: Symbolic<Vector3f>) : Symbolic<Vector3f> {
	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			VectorSum(a.substitute(find, replace), b.substitute(find, replace))

	override fun eval() = a.eval().add(b.eval())

	override val variables
		get() = a.variables + b.variables

}

data class GetX(override val arg: Symbolic<Vector3f>) : UnarySymbolicOp<Vector3f, Scalar> {
	override fun createOp(arg: Symbolic<Vector3f>) = GetX(this.arg)

	override fun eval() = arg.eval().x
}

data class GetY(override val arg: Symbolic<Vector3f>) : UnarySymbolicOp<Vector3f, Scalar> {
	override fun createOp(arg: Symbolic<Vector3f>) = GetY(this.arg)

	override fun eval() = arg.eval().y
}

data class GetZ(override val arg: Symbolic<Vector3f>) : UnarySymbolicOp<Vector3f, Scalar> {
	override fun createOp(arg: Symbolic<Vector3f>) = GetZ(this.arg)

	override fun eval() = arg.eval().z
}

data class VectorDifference(val a: Symbolic<Vector3f>, val b: Symbolic<Vector3f>) : Symbolic<Vector3f> {
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
	get() = GetX(this)

private val Symbolic<Vector3f>.y
	get() = GetY(this)

private val Symbolic<Vector3f>.z
	get() = GetZ(this)

operator fun CSymVector3.plus(opB: CSymVector3) = VectorSum(this, opB)

