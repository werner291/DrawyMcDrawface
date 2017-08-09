package nl.wernerkroneman.SymboliK

/**
 *
 * Base interface of Symbolic expressions.
 *
 * @param T The type to be returned by the simplify() and substitute() methods.
 *
 */
interface Symbolic<T : Symbolic<T>> {

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
	val variables: Set<Variable>

	/**
	 * Substitute one (sub)expression by another, essentially find-replace.
	 *
	 * Works recursively on non-matches, but not on the replaced result.
	 *
	 * Note that the type of the expression is significant.
	 */
	@Suppress("UNCHECKED_CAST")
	fun <V : Symbolic<V>> substitute(find: V, replace: V): T =
			if (this == find)
			// Should be safe since equality includes arg typecheck
				replace as T
			else
				substituteInside(find, replace)

	fun <V : Symbolic<V>> substituteInside(find: V, replace: V): T

	fun simplify(depth: Int): T

	fun simplifyFully(): T = simplify(Int.MAX_VALUE)

}

/**
 * Symbolic representing arg constant value of type T.

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
}*/

/**
 * A named variable
 *
 * Throughout an expression, variables with the same name
 * and type (==) are considered to be the same variable.
 */
interface Variable {

	val name: String

}