package nl.wernerkroneman.DrawyMcDrawface

import nl.wernerkroneman.SymboliK.*

private val x = Variable<Scalar>("x")
private val y = Variable<Scalar>("y")
private val z = Variable<Scalar>("z")

val xyAlignedUnitSquare = CSymFace(
		CSymVector3(ScalarC(-0.5f) + x,
					ScalarC(-0.5f) + y,
					ScalarC(0.0f) + z),
		CSymVector3(ScalarC(-0.5f) + x,
					ScalarC(0.5f) + y,
					ScalarC(0.0f) + z),
		CSymVector3(ScalarC(0.5f) + x,
					ScalarC(0.5f) + y,
					ScalarC(0.0f) + z),
		CSymVector3(ScalarC(0.5f) + x,
					ScalarC(-0.5f) + y,
					ScalarC(0.0f) + z)
)

// TODO use symbolics more strongly
val xyAlignedUnitRadiusDisk = CSymFace(CSymIterable((0..15).map {
	CSymVector3(ScalarC(Math.cos(Math.PI * 2.0 * it / 16.0) * 0.5) + x,
				ScalarC(Math.sin(Math.PI * 2.0 * it / 16.0) * 0.5) + y,
				ScalarC(0.0f) + z)
}))

val axisAlignedUnitCube = xyAlignedUnitSquare
		.extrude(CSymVector3(ScalarC(0.0f),
							 ScalarC(0.0f),
							 ScalarC(1.0f)))
		.simplifyFully()

val zAxisAlignedCylinder = xyAlignedUnitRadiusDisk
		.extrude(CSymVector3(ScalarC(0.0f),
							 ScalarC(0.0f),
							 VarScalar("height")))
		.simplifyFully()