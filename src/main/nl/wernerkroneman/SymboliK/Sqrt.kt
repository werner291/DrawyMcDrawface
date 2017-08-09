package nl.wernerkroneman.SymboliK


/**
 * Represents a sqrt() operation on scalars.
 */
data class Sqrt(val param: SymScalar) : SymScalar {

	override fun simplify(depth: Int): SymScalar {
		return Sqrt(param.simplify(depth))
	}

	override val variables: Set<Variable>
		get() = param.variables

	override fun <V : Symbolic<V>> substituteInside(find: V,
													replace: V): Sqrt {
		return Sqrt(param.substitute(find, replace))
	}

}