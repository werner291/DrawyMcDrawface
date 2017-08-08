package rendering

import org.joml.Vector3f

typealias Edge = Pair<Vector3f, Vector3f>

data class Mesh(val vertices: List<Vector3f>,
				val normals: List<Vector3f> = autocomputeNormals(vertices)) {

	constructor(faces: Iterable<Face>) : this(
			// TODO: can we do better than this triangle-fan triangulation?
			faces.flatMap { face ->
				face.vertices
						.drop(1)
						.pairwise()
						.flatMap {
							listOf(face.vertices.first(),
								   it.first,
								   it.second)
						}
						.map {
							Vector3f(it.x.toFloat(),
									 it.y.toFloat(),
									 it.z.toFloat())
						}
			})

	val edges: Iterable<Edge>
		get() = vertices.chunked(3).flatMap { (it + it.first()).pairwise() }

	val faces: Iterable<Face>
		get() = vertices.chunked(3).map { Face(it) }
}

fun autocomputeNormals(vertices: List<Vector3f>) =
		vertices.chunked(3)
				.map {
					it[0].sub(it[1], Vector3f())
							.cross(it[2].sub(it[1], Vector3f())).normalize()
				}
				.flatMap { listOf(it, it, it) }
