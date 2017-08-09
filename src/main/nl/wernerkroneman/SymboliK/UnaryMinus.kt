package nl.wernerkroneman.SymboliK

/**
 * Created by werner on 9-8-17.
 */
data class Negation(val a: SymScalar) : SymScalar {
	override val variables: Set<Variable>
		get() = a.variables

	override fun <V : Symbolic<V>> substituteInside(find: V, replace: V): SymScalar {
		return Negation(a.substitute(find, replace))
	}

	override fun simplify(depth: Int): SymScalar {
		TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
	}

}