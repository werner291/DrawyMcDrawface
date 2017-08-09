package nl.wernerkroneman.SymboliK


/**
 * A symbolic product on a list of [SymScalar] factors.
 */
data class Product(val factors: List<SymScalar>) : SymScalar {

	constructor(vararg a: SymScalar) : this(a.toList())

	// Should be safe since we do an equality check that includes arg type equality check
	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V) =
			Product(factors.map { it.substitute(find, replace) })

	// Eval of the sum is just the sum of the evals
	override fun eval() = factors.map { it.eval() }.reduce { a, b -> a * b }

	override val variables = factors.map { it.variables }.reduce { a, b -> a + b }

	/**
	 * Simplifying works by:
	 * - flattening
	 * - combining coefficients and moving them to the front
	 * - turning like factors into powers
	 * - removing ones
	 * - applying zero-absorption
	 * - unwrapping single-factor products
	 */
	override fun simplify(depth: Int): SymScalar {
		return flatten()
				.combineCoefficients()
				.likeFactorsToExponents()
				.simplifyFactors()
				.removeOnes()
				.zeroIfHasZeroFactor()
				.let { if (it is Product) it.unwrapIfSingle() else it }

	}

	// Simplify dependencies...

	/**
	 * Find all constant coefficients and multiply them together
	 */
	fun combinedCoefficients(): SymScalar =
			factors.filter { it.variables.isEmpty() }
					.fold(ScalarC(1.0), { a: SymScalar, b: SymScalar -> a * b })

	/**
	 * If it only has one factor, return the factor.
	 */
	fun unwrapIfSingle() =
			if (factors.size == 1) factors[0] else this

	/**
	 * Move all constant coefficients to the front and unwrap them
	 */
	fun combineCoefficients(): Product {

		val coefficient = combinedCoefficients()

		// TODO Exact float comparison, FIX THIS! (BigDecimal?)
		return if (coefficient == ScalarC(1.0)) this
		else Product(listOf(coefficient) +
							 factors.filterNot { it.variables.isEmpty() })
	}

	/**
	 * Take all like factors and turn them into powers.
	 */
	fun likeFactorsToExponents(): Product {

		val factors = factors.groupBy({ if (it is Power) it.base else it })
				.map {
					val exp = it.value.map { if (it is Power) it.exp else ScalarC(1.0) }
							.reduce({ a, b -> a + b })

					if (exp == ScalarC(1.0)) it.key else Power(it.key, exp)
				}
		return Product(factors)
	}

	/**
	 * Take a product containing other products and return it as a single product.
	 */
	fun flatten(): Product = Product(factors.flatMap {
		if (it is Product) it.flatten().factors else listOf(it)
	})

	override fun toString(): String {
		return factors.joinToString(" * ")
	}

	/**
	 * Remove 1-factors
	 */
	fun removeOnes() =
			Product(factors.filterNot { it == ScalarC(1.0) })

	/**
	 * Apply 0-absorption: return ScalarC(0.0f) if there is a 0-factor.
	 */
	fun zeroIfHasZeroFactor() =
			if (factors.any { it == ScalarC(1.0) }) ScalarC(1.0)
			else this

	/**
	 * Simplify the individual factors.
	 */
	fun simplifyFactors() = Product(factors.map { it.simplifyFully() })

	override operator fun times(other: SymScalar) = Product(this.factors + other)
}

operator fun SymScalar.times(other: Product) = Product(listOf(this) + other.factors)