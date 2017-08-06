package nl.wernerkroneman.SymboliK

interface BinarySymbolicOp<InTypeA : Any,
		InTypeB : Any,
		OutType : Any> :
		Symbolic<OutType> {

	val argA: Symbolic<InTypeA>
	val argB: Symbolic<InTypeB>

	fun createOp(argA: Symbolic<InTypeA>,
				 argB: Symbolic<InTypeB>): BinarySymbolicOp<InTypeA, InTypeB, OutType>

	override val variables
		get() = argA.variables + argB.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Symbolic<OutType> {
		return createOp(argA.substitute(find, replace),
						argB.substitute(find, replace))
	}
}