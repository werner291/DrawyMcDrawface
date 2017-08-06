package symbolic

import rendering.Scalar

/**
 * Represents raising a symbolic scalar to a symbolic scalar power.
 */
data class Power(val base: SymScalar, val exp: SymScalar) : SymScalar {

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			Power(base.substitute(find, replace), exp.substitute(find, replace))

	override fun eval() = Math.pow(base.eval().toDouble(),
								   exp.eval().toDouble()).toFloat()

	override fun toString(): String {
		return "$base^$exp"
	}

	override val variables = base.variables + exp.variables

	override fun simplify(): Symbolic<Scalar> {
		// Simplify the base and exponent only.
		return Power(base.simplify(), exp.simplify())
	}
}