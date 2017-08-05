package symbolic

import rendering.Scalar

data class Sqrt(val param: SymScalar) : SymScalar {
	override fun eval(): Scalar {
		TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
	}

	override val variables: Set<Variable<out Any>>
		get() = TODO("not implemented") //To change initializer of created properties use File | Settings | File Templates.

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Sqrt {
		TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
	}

}