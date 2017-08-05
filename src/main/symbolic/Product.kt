package symbolic

import rendering.Scalar


/**
 * A symbolic sum on arg Double and some other number.
 */
data class Product(val a: SymScalar, val b: SymScalar) : SymScalar {

	// Should be safe since we do an equality check that includes arg type equality check
	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			Product(a.substitute(find, replace), b.substitute(find, replace))

	// Eval of the sum is just the sum of the evals
	override fun eval() = a.eval() * b.eval()

	override val variables = a.variables + b.variables
}

operator fun SymScalar.times(other: SymScalar) = Product(this, other)
operator fun SymScalar.times(other: Scalar) = Product(this, ScalarC(other))