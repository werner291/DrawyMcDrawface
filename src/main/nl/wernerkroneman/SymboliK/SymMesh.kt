package nl.wernerkroneman.SymboliK

import org.joml.Vector3f
import rendering.Edge
import rendering.Face
import rendering.Mesh
import rendering.Vector3

data class SymEdge(val a: Symbolic<Vector3f>,
				   val b: Symbolic<Vector3f>) : Symbolic<Pair<Vector3f, Vector3f>> {

	override fun eval(): Pair<Vector3f, Vector3f> {
		return a.eval() to b.eval()
	}

	fun length() = (a - b).length()

	override val variables: Set<Variable<out Any>>
		get() = a.variables + b.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) =
			SymEdge(a.substitute(find, replace), b.substitute(find, replace))

}


private val it = Variable<Vector3>("it")

val translatedFace = CSymFace(MappedIterable(Variable<Face>("face").faceVertices,
											 VectorSum(it, Variable<Vector3>("translation")), it))

operator fun Symbolic<Vector3>.plus(variable: Symbolic<Vector3>) =
		VectorSum(this, variable)

fun Symbolic<Face>.translate(by: Symbolic<Vector3f>) =
		translatedFace.substitute(Variable<Vector3>("translation"), by)
				.substitute(Variable<Face>("face"), this)

// TODO: possibly define as composition of the other functions
data class Extrusion(val face: SymFace, val dir: Symbolic<Vector3>) :
		BinarySymbolicOp<Face, Vector3, Mesh> {

	override val argA: Symbolic<Face> = face
	override val argB: Symbolic<Vector3> = dir

	override fun createOp(argA: Symbolic<Face>, argB: Symbolic<Vector3>) =
			Extrusion(face, dir)

	override fun eval(): Mesh {

		val cap = face.eval()
		val end = face.translate(dir).eval()

		return Mesh(listOf(cap, end) + cap.edges.zip(end.edges).map { (e1, e2) ->
			Face(listOf(e1.first, e1.second, e2.second, e1.first))
		})

	}

}

fun SymFace.extrude(dir: Symbolic<Vector3f>): Extrusion {
	return Extrusion(this, dir)
}

data class SymMesh(val faces: SymIterable<Face>) : Symbolic<Mesh> {

	val edges: SymIterable<Edge>
		get() = faces.map(Variable<Face>("face").faceEdges,
						  Variable<rendering.Face>("face")).flatten()

	val vertices: SymIterable<Vector3>
		get() = faces.map(Variable<Face>("it").faceVertices).flatten()

	override fun eval() = Mesh(faces.eval())

	override val variables: Set<Variable<out Any>>
		get() = faces.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) =
			SymMesh(faces.substitute(find, replace))

}

data class GetVertices(override val arg: Symbolic<Mesh>) :
		UnarySymbolicOp<Mesh, Iterable<Vector3>> {

	override fun createOp(mesh: Symbolic<Mesh>) = GetVertices(mesh)

	override fun eval(): Iterable<Vector3f> {
		return arg.eval().vertices
	}
}

data class GetFaceVertices(override val arg: Symbolic<Face>) :
		UnarySymbolicOp<Face, Iterable<Vector3>> {

	override fun createOp(arg: Symbolic<Face>) = GetFaceVertices(arg)

	override fun eval(): Iterable<Vector3f> {
		return arg.eval().vertices
	}
}

val Symbolic<Mesh>.vertices: SymIterable<Vector3>
	get() = GetVertices(this)

val Symbolic<Face>.faceVertices: SymIterable<Vector3>
	get() = GetFaceVertices(this)

data class GetEdges(override val arg: Symbolic<Mesh>) :
		UnarySymbolicOp<Mesh, Iterable<Edge>> {

	override fun createOp(mesh: Symbolic<Mesh>) = GetEdges(mesh)

	override fun eval(): Iterable<Edge> {
		return arg.eval().edges
	}
}

val Symbolic<Mesh>.edges: Symbolic<Iterable<Edge>>
	get() = GetEdges(this)

val Symbolic<Face>.faceEdges: Symbolic<Iterable<Edge>>
	get() = Getter<Face, Iterable<Edge>>("edges", this, { it.edges })

data class GetFaces(override val arg: Symbolic<Mesh>) :
		UnarySymbolicOp<Mesh, Iterable<Face>> {

	override fun createOp(mesh: Symbolic<Mesh>) = GetFaces(mesh)

	override fun eval(): Iterable<Face> {
		return arg.eval().faces
	}
}

val Symbolic<Mesh>.faces: Symbolic<Iterable<Face>>
	get() = GetFaces(this)