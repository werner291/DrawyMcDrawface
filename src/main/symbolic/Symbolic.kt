package symbolic

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

	// Possibly replace with arg simplify() call that reduces to arg single Const
	fun eval(): T

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

	fun simplify(): Symbolic<T> {
		return this
	}

}

/**
 * Symbolic representing arg constant value of type T.
 */
interface Const<T : Any> : Symbolic<T> {

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

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) = this

	override fun eval(): T {
		throw UnsupportedOperationException(
				"Cannot evaluate arg symbolic expression with variables.")
	}

	override val variables: Set<Variable<out Any>> = setOf(this)
}

data class Getter<T : Any, R : Any>(val name: String,
									val getFrom: Symbolic<T>,
									val fromEval: (T) -> R) : UnarySymbolicOp<T, R> {
	override val arg = getFrom

	override fun createOp(arg: Symbolic<T>): Symbolic<R> {
		return Getter<T, R>(name, arg, fromEval)
	}

	override fun eval(): R {
		return fromEval(getFrom.eval())
	}
}