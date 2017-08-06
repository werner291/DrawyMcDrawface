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

	override val variables
		get() = terms.map { it.variables }
			.fold(emptySet<Variable<out Any>>(), { a, b -> a + b })

	override fun simplify(): Symbolic<Scalar> {
		return this.flatten().collectTerms().simplifyTerms().unwrapIfSingle()
	}

	fun flatten(): Sum = Sum(terms.flatMap {
		if (it is Sum) it.flatten().terms else listOf(it)
	})

	override fun toString(): String {
		return "(${terms.joinToString(" + ")})"
	}

	fun collectTerms(): Sum {

		val groupedTerms = terms.groupBy {
			if (it is Product)
				Product(it.factors.filterNot { it.variables.isEmpty() }).unwrapIfSingle()
			else it
		}.map {
			it.value.fold(ScalarC(0.0f)) {
				acc: ScalarC, sym ->
				ScalarC(acc.eval() +
								if (sym is Product) {
									sym.combinedCoefficients()
								} else
									1.0f
				)
			} * it.key
		}.filterNot {
			it.zeroIfHasZeroFactor() == ScalarC(0.0f)
		}

		return Sum(groupedTerms)

	}


	fun simplifyTerms() = Sum(terms.map { it.simplify() })

	fun unwrapIfSingle() =
			if (terms.size == 1) terms[0] else this
}

operator fun Sum.plus(other: SymScalar) = Sum(this.terms + other)
operator fun SymScalar.plus(other: Sum) = Sum(listOf(this) + other.terms)

operator fun SymScalar.plus(other: SymScalar) = Sum(this, other)

operator fun SymScalar.plus(other: Scalar) = this + ScalarC(other)
operator fun SymScalar.plus(d: Double) = this + ScalarC(d)

operator fun SymScalar.minus(other: SymScalar) = this + (-other)
operator fun SymScalar.minus(other: Scalar) = this + ScalarC(-other)

private operator fun SymScalar.unaryMinus() = ScalarC(-1) * this
