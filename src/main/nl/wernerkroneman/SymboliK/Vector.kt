package nl.wernerkroneman.SymboliK

interface SymVector3 : Symbolic<SymVector3> {

	operator fun plus(other: SymVector3) = VectorSum(this, other)

	operator fun times(other: SymScalar) = ScalarProduct(this, other)
	operator fun times(other: Double) = this * ScalarC(other)
	operator fun times(other: Number) = this * ScalarC(other.toDouble())

	operator fun minus(other: SymVector3) = this + (-other)

	operator fun unaryMinus() = this * ScalarC(-1.0)

	val x: SymScalar get() = ScalarGetter<SymVector3>({ it.x }, this)
	val y: SymScalar get() = ScalarGetter<SymVector3>({ it.y }, this)
	val z: SymScalar get() = ScalarGetter<SymVector3>({ it.z }, this)

	fun SymVector3.norm(): SymScalar {
		// TODO use some unary op data class, optionally, simplifying?
		return Sqrt(this.x * this.x + this.y * this.y + this.z * this.z)
	}

}

data class CSymVector3(override val x: SymScalar,
					   override val y: SymScalar,
					   override val z: SymScalar) : SymVector3 {

	override fun simplify(depth: Int): SymVector3 {
		return CSymVector3(x.simplify(depth - 1), y.simplify(depth - 1), z.simplify(depth - 1))
	}

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V) =
			CSymVector3(x.substitute(find, replace),
						y.substitute(find, replace),
						z.substitute(find, replace))

	override val variables = x.variables + y.variables + z.variables
}

data class ScalarProduct(val vector: SymVector3, val scalar: SymScalar) : SymVector3 {
	override val variables: Set<Variable>
		get() = vector.variables + scalar.variables

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V): SymVector3 {
		return ScalarProduct(vector.substitute(find, replace),
							 scalar.substitute(find, replace))
	}

	override fun simplify(depth: Int): SymVector3 {
		return ScalarProduct(vector.simplify(depth), scalar.simplify(depth))
	}
}

data class DotProduct(val a: SymVector3, val b: SymVector3) : SymScalar {
	override val variables: Set<Variable>
		get() = a.variables + b.variables

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V): SymScalar {
		return DotProduct(a.substitute(find, replace),
						  b.substitute(find, replace))
	}

	override fun simplify(depth: Int): SymScalar {
		return DotProduct(a.simplify(depth), b.simplify(depth))
	}
}

data class VectorSum(val a: SymVector3, val b: SymVector3) : SymVector3 {
	override fun simplify(depth: Int): SymVector3 {

		val simpleA = a.simplify(depth - 1)
		val simpleB = b.simplify(depth - 1)

		return if (simpleA is CSymVector3 && simpleB is CSymVector3)
			CSymVector3(simpleA.x + simpleB.x,
						simpleA.y + simpleB.y,
						simpleA.z + simpleB.z).simplify(depth - 1)
		else
			VectorSum(a.simplify(depth - 1), b.simplify(depth - 1))
	}

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V) =
			VectorSum(a.substitute(find, replace), b.substitute(find, replace))

	override val variables
		get() = a.variables + b.variables

}