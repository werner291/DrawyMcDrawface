package symbolic

interface UnarySymbolicOp<InType : Any, OutType : Any> : Symbolic<OutType> {

	val arg: Symbolic<InType>

	fun createOp(arg: Symbolic<InType>): Symbolic<OutType>

	override val variables
		get() = arg.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Symbolic<OutType> {
		return createOp(arg.substitute(find, replace))
	}
}


