package nl.wernerkroneman.SymboliK

/**
 * Represents raising a symbolic scalar to a symbolic scalar power.
 */
data class Power(val base: SymScalar, val exp: SymScalar) : SymScalar {

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V) =
			Power(base.substitute(find, replace), exp.substitute(find, replace))

	override fun toString(): String {
		return "$base^$exp"
	}

	override val variables = base.variables + exp.variables

	override fun simplify(depth: Int): SymScalar {
		// Simplify the base and exponent only.
		return Power(base.simplify(depth - 1), exp.simplify(depth - 1))
	}
}