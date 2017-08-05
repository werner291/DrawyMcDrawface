package symbolic

import org.joml.Vector3f
import rendering.Edge
import rendering.Face

typealias SymFace = Symbolic<Face>

data class CSymFace(val vertices: SymIterable<out Vector3f>) : Symbolic<Face> {

	constructor(vararg vertices: Symbolic<out Vector3f>) :
			this(CSymIterable(vertices.toList()))

	override fun eval() = Face(vertices.eval().toList())

	val edges: SymIterable<Edge>
		get() = (vertices.append(vertices.first()))
				.pairwise()

	override val variables: Set<Variable<out Any>>
		get() = vertices.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) =
			CSymFace(vertices.substitute(find, replace))

}