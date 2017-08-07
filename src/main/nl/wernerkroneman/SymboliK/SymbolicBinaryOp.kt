package nl.wernerkroneman.SymboliK

data class SymbolicBinaryOp<A : Any, B : Any, R : Any>(
		val a: Symbolic<A>, val b: Symbolic<B>,
		val simplify: (Symbolic<A>, Symbolic<B>, Int) -> Symbolic<R>) : Symbolic<R> {

	override fun simplify(depth: Int): Symbolic<R> {

		return simplify(a.simplifyFully(),
						b.simplifyFully(), depth)
	}

	override val variables: Set<Variable<out Any>>
		get() = a.variables + b.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Symbolic<R> {
		return this.copy(a = a.substitute(find, replace), b = b.substitute(find, replace))
	}
}

data class SymbolicUnaryOp<A : Any, R : Any>(
		val a: Symbolic<A>,
		val simplify: (Symbolic<A>, Int) -> Symbolic<R>) : Symbolic<R> {

	override fun simplify(depth: Int): Symbolic<R> {
		return simplify(a.simplifyFully(), depth)
	}

	override val variables: Set<Variable<out Any>>
		get() = a.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Symbolic<R> {
		return this.copy(a = a.substitute(find, replace))
	}

}