package nl.wernerkroneman.DrawyMcDrawface

import nl.wernerkroneman.SymboliK.Symbolic
import nl.wernerkroneman.SymboliK.Variable
import nl.wernerkroneman.SymboliK.length
import nl.wernerkroneman.SymboliK.minus
import org.joml.Vector3f

data class CSymEdge(val a: Symbolic<Vector3f>,
					val b: Symbolic<Vector3f>) : Symbolic<Pair<Vector3f, Vector3f>> {

	override fun simplify(depth: Int): Symbolic<Pair<Vector3f, Vector3f>> {
		return CSymEdge(a.simplify(depth - 1), b.simplify(depth - 1))
	}

	override fun eval(): Pair<Vector3f, Vector3f> {
		return a.eval() to b.eval()
	}

	fun length() = (a - b).length()

	override val variables: Set<Variable<out Any>>
		get() = a.variables + b.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>) =
			CSymEdge(a.substitute(find, replace), b.substitute(find, replace))

}