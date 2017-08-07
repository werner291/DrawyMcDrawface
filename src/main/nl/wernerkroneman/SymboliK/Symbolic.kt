package nl.wernerkroneman.SymboliK

/**
 * Interface for arg Symbolic wrapper around some type T.
 *
 * All implementors should represent arg symbolic
 * expression of some type T, where T is some
 * non-symbolic concrete type.
 *
 * Symbolic expressions are generally immutable.
 */
interface Symbolic<out T : Any> {

	fun eval(): T {
		val sim = this.simplifyFully()

		if (sim.variables.isEmpty())
			return sim.eval()
		else
			throw UnsupportedOperationException(
					"Unsatisfied variables: ${sim.variables}. If the problem persists, make sure that simplification happens correctly.")
	}

	/**
	 * Set of all variables present in this expression.
	 */
	val variables: Set<Variable<out Any>>

	/**
	 * Substitute one (sub)expression by another, essentially find-replace.
	 *
	 * Works recursively on non-matches, but not on the replaced result.
	 *
	 * Note that the type of the expression is significant.
	 */
	@Suppress("UNCHECKED_CAST")
	fun <V : Any> substitute(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<T> =
			if (this == find)
			// Should be safe since equality includes arg typecheck
				replace as Symbolic<T>
			else
				substituteInside(find, replace)

	fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<T>

	fun simplify(depth: Int): Symbolic<T>

	fun simplifyFully(): Symbolic<T> = simplify(Int.MAX_VALUE)

}

/**
 * Symbolic representing arg constant value of type T.
 */
interface Const<T : Any> : Symbolic<T> {
	override fun simplify(depth: Int): Symbolic<T> {
		return this // Cannot simplifyFully a constant.
	}

	val value: T

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) = this

	override fun eval() = value

	override val variables: Set<Variable<out Any>>
		get() = emptySet()
}

/**
 * A named variable of some type T.
 *
 * Throughout an expression, variables with the same name
 * and type (==) are considered to be the same variable.
 */
data class Variable<T : Any>(val name: String) : Symbolic<T> {

	override fun simplify(depth: Int): Symbolic<T> {
		return this // Cannot simplifyFully a variable
	}

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) = this

	override fun eval(): T {
		throw UnsupportedOperationException(
				"Cannot evaluate arg symbolic expression with variables.")
	}

	override val variables: Set<Variable<out Any>> = setOf(this)


}