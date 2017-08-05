package rendering

import org.joml.Vector3f

data class Face(val vertices: List<Vector3f>) {

	val edges
		get() = (vertices + vertices.first()).pairwise()

	fun translate(translation: Vector3f) =
			Face(vertices.map { it.add(translation) })
}