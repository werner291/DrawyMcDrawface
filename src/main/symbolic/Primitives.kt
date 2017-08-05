package symbolic

import rendering.Scalar

private val x = Variable<Scalar>("x")
private val y = Variable<Scalar>("y")
private val z = Variable<Scalar>("z")

val xyAlignedUnitSquare = CSymFace(
		CSymVector3(ScalarC(-0.5f) + x, ScalarC(-0.5f) + y, ScalarC(0.0f) + z),
		CSymVector3(ScalarC(0.5f) + x, ScalarC(-0.5f) + y, ScalarC(0.0f) + z),
		CSymVector3(ScalarC(0.5f) + x, ScalarC(0.5f) + y, ScalarC(0.0f) + z),
		CSymVector3(ScalarC(-0.5f) + x, ScalarC(-0.5f) + y, ScalarC(0.0f) + z)
)

val axisAlignedUnitCube = xyAlignedUnitSquare
		.extrude(CSymVector3(ScalarC(0.0f), ScalarC(0.0f), ScalarC(1.0f)))
		.simplify()