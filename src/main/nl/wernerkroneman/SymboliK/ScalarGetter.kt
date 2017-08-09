package nl.wernerkroneman.SymboliK

data class ScalarGetter<T : Symbolic<T>>(val simplify: (T) -> SymScalar,
										 val getFrom: T) : SymScalar {

	override val variables: Set<Variable>
		get() = getFrom.variables

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V): SymScalar {
		return ScalarGetter<T>(simplify, getFrom.substitute(find, replace))
	}

	override fun simplify(depth: Int) = simplify(getFrom)

}