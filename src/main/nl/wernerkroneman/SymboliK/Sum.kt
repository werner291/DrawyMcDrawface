package nl.wernerkroneman.SymboliK

/**
 * A symbolic sum on arg Double and some other number.
 */
data class Sum(val terms: List<SymScalar>) : Symbolic<SymScalar> {

	override fun <V : Symbolic<V>> substituteInside(find: V,
													replace: V): SymScalar {
		return Sum(terms.map { it.substitute(find, replace) })
	}

	constructor(vararg terms: SymScalar) : this(terms.toList())

	override val variables
		get() = terms.map { it.variables }
				.fold(emptySet<Variable>(), { a, b -> a + b })

	override fun simplify(depth: Int): SymScalar {
		return this.flatten()
				.collectTerms()
				.simplifyTerms()
				.removeZeros()
				.unwrapIfSingle()
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
			val coeff: SymScalar = it.value.fold(ScalarC(0.0)) {
				acc: SymScalar, sym: SymScalar ->
				acc + if (sym is Product)
									sym.combinedCoefficients()
				else
					ScalarC(1.0)

			}

			when (coeff) {
				ScalarC(0.0) -> null
				ScalarC(1.0) -> it.key
				else -> coeff * it.key
			}
		}.filterNotNull()

		return if (groupedTerms.isEmpty()) Sum(ScalarC(0.0))
		else Sum(groupedTerms)

	}

	fun simplifyTerms() = Sum(terms.map { it.simplifyFully() })

	fun removeZeros() = Sum(terms.filterNot { it == ScalarC(0.0) }
									.takeIf { !it.isEmpty() } ?: listOf(ScalarC(0.0)))

	fun unwrapIfSingle() =
			if (terms.size == 1) terms[0] else this
}

operator fun Sum.plus(other: SymScalar) = Sum(this.terms + other)
operator fun SymScalar.plus(other: Sum) = Sum(listOf(this) + other.terms)