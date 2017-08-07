package nl.wernerkroneman.SymboliK

typealias VarScalar = Variable<Scalar>

typealias SymScalar = Symbolic<Scalar>

data class ScalarC(override val value: Scalar) : Const<Scalar> {
	// TODO: implicit conversion to float is bad.
	constructor(value: Double) : this(value.toFloat())

	constructor(value: Int) : this(value.toFloat())
}

data class IntC(override val value: Int) : Const<Int>