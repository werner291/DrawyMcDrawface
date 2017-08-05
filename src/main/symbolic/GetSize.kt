package symbolic

data class GetSize<T>(val sizeOf: Symbolic<Iterable<T>>) : Symbolic<Int> {
	override fun eval(): Int {
		return sizeOf.eval().count()
	}

	override val variables: Set<Variable<out Any>>
		get() = sizeOf.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<Int> {
		return GetSize<T>(sizeOf.substitute(find, replace))
	}
}