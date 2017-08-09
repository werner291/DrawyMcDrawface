package nl.wernerkroneman.SymboliK

import org.joml.Vector3f

typealias Scalar = Float

typealias Vector3 = Vector3f

interface SymScalar : Symbolic<SymScalar> {

	operator fun plus(other: SymScalar) = Sum(this, other)
	operator fun plus(other: Double) = this + ScalarC(other)
	operator fun plus(other: Number) = this + ScalarC(other.toDouble())

	operator fun times(other: SymScalar) = Product(this, other)
	operator fun times(other: Double) = this * ScalarC(other)
	operator fun times(other: Number) = this * ScalarC(other.toDouble())

	operator fun minus(other: SymScalar) = this + (-other)

	operator fun unaryMinus() = Negation(this)

	operator fun minus(other: Int) = this + (-other)
	operator fun minus(other: Double) = this + (-other)

}

data class ScalarC(val value: Double) : SymScalar {
	override val variables: Set<Variable>
		get() = emptySet()

	override fun <V : Symbolic<V>> substituteInside(find: V,
													replace: V): SymScalar {
		return this // Constants have no "inside"
	}

	override fun simplify(depth: Int): SymScalar {
		return this // Constants cannot be simplified
	}
}

data class VarScalar(override val name: String) : SymScalar, Variable {
	override val variables: Set<Variable>
		get() = setOf(this)

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V): SymScalar {
		return this // Variables have non "inside"
	}

	override fun simplify(depth: Int): SymScalar {
		return this // Variables cannot be simplified
	}
}