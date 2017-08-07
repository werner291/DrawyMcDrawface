package nl.wernerkroneman.DrawyMcDrawface

import nl.wernerkroneman.SymboliK.*
import org.joml.Vector3f
import rendering.Edge
import rendering.Face
import rendering.Mesh

private val it = Variable<Vector3>("it")
private val face = Variable<Face>("face")
private val edge = Variable<Edge>("edge")
private val translation = Variable<Vector3>("translation")

val translatedFace = CSymFace(face.faceVertices
									  .map(VectorSum(it, Variable<Vector3>("translation")), it))

operator fun Symbolic<Vector3>.plus(variable: Symbolic<Vector3>) =
		VectorSum(this, variable)


fun Symbolic<Face>.translate(by: Symbolic<Vector3f>): Symbolic<Face> {
	return translatedFace.substitute(translation, by)
			.substitute(face, this)
}

val extrudedFace = CSymMesh(
		CSymIterable(face,
					 face.translate(translation)).concat(
				face.faceEdges.map(CSymFace(edge.first,
											edge.second,
											edge.second + translation,
											edge.first + translation),
								   edge)
		)
)

fun SymFace.extrude(dir: Symbolic<Vector3f>) =
		extrudedFace.substitute(face, this).substitute(
				translation, dir)

data class CSymMesh(val faces: SymIterable<Face>) : Symbolic<Mesh> {

	override fun simplify(depth: Int): Symbolic<Mesh> {
		return CSymMesh(faces.simplify(depth - 1))
	}

	val edges: SymIterable<Edge>
		get() = faces.map(Variable<Face>("face").faceEdges,
						  Variable<Face>("face"))
				.distinct().flatten()

	val vertices: SymIterable<Vector3>
		get() = faces.map(Variable<Face>("it").faceVertices).flatten().distinct()

	override fun eval() = Mesh(faces.eval())

	override val variables: Set<Variable<out Any>>
		get() = faces.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) =
			CSymMesh(faces.substitute(find, replace))

}

val Symbolic<Mesh>.vertices: SymIterable<Vector3>
	get() = getter(CSymMesh::vertices, this)

val Symbolic<Face>.faceVertices: SymIterable<Vector3>
	get() = getter(CSymFace::vertices, this)

val Symbolic<Mesh>.edges: Symbolic<Iterable<Edge>>
	get() = getter(CSymMesh::edges, this)

val Symbolic<Face>.faceEdges: Symbolic<Iterable<Edge>>
	get() = getter(CSymFace::edges, this)

val Symbolic<Mesh>.faces: Symbolic<Iterable<Face>>
	get() = getter(CSymMesh::faces, this)