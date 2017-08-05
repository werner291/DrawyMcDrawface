package symbolic

import rendering.Scalar

/**
 * A symbolic sum on arg Double and some other number.
 */
data class Sum(val terms: List<SymScalar>) : SymScalar {

	constructor(vararg terms: SymScalar) : this(terms.toList())

	// Should be safe since we do an equality check that includes arg type equality check
	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>) =
			Sum(terms.map { it.substitute(find, replace) })

	// Eval of the sum is just the sum of the evals
	override fun eval() = terms.map { it.eval() }.sum()

	override val variables = terms.map { it.variables }
			.fold(emptySet<Variable<out Any>>(), { a, b -> a + b })

	override fun simplify(): Symbolic<Scalar> {
		return this.flatten()
	}

	fun flatten(): Sum = Sum(terms.flatMap {
		if (it is Sum) it.flatten().terms else listOf(it)
	})

	override fun toString(): String {
		return terms.joinToString(" + ")
	}

	fun collectTerms() = Sum(emptyList())


}

operator fun Sum.plus(other: SymScalar) = Sum(this.terms + other)
operator fun SymScalar.plus(other: Sum) = Sum(listOf(this) + other.terms)
operator fun SymScalar.plus(other: SymScalar) = Sum(this, other)
operator fun SymScalar.plus(other: Scalar) = Sum(this, ScalarC(other))
operator fun SymScalar.plus(d: Double) = this + ScalarC(d)

operator fun SymScalar.minus(other: SymScalar) = Sum(this, -other)
operator fun SymScalar.minus(other: Scalar) = Sum(this, ScalarC(-other))

private operator fun SymScalar.unaryMinus() = ScalarC(-1) * this
