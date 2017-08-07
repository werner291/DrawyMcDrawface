package nl.wernerkroneman.SymboliK

data class CSymPair<A : Any, B : Any>(val a: Symbolic<A>,
									  val b: Symbolic<B>) : Symbolic<Pair<A, B>> {

	override fun eval(): Pair<A, B> {
		return a.eval() to b.eval()
	}

	override val variables: Set<Variable<out Any>>
		get() = a.variables + b.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) =
			CSymPair(a.substitute(find, replace), b.substitute(find, replace))

	override fun simplify(depth: Int): Symbolic<Pair<A, B>> {
		return CSymPair<A, B>(a.simplify(depth - 1), b.simplify(depth - 1))
	}
}

val <A : Any, B : Any>Symbolic<Pair<A, B>>.first: Symbolic<A>
	get() = getter(CSymPair<A, B>::a, this)

val <A : Any, B : Any>Symbolic<Pair<A, B>>.second: Symbolic<B>
	get() = getter(CSymPair<A, B>::b, this)