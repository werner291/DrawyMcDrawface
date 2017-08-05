package symbolic

import rendering.Scalar

class ScalarC(value: Scalar) : Const<Scalar>(value) {
	// TODO: implicit conversion to float is bad.
	constructor(value: Double) : this(value.toFloat())

	constructor(value: Int) : this(value.toFloat())
}