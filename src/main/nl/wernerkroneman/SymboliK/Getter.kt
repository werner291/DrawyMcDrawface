package nl.wernerkroneman.SymboliK

import kotlin.reflect.KClass

@Suppress("UNCHECKED_CAST") // We use a runtime type check.
data class Getter<R : Any,
		CType : Symbolic<T>,
		T : Any>(val deconstruct: (CType) -> Symbolic<R>,
				 val symbolic: Symbolic<T>,
				 val cType: KClass<CType>) : Symbolic<R> {

	override val variables: Set<Variable<out Any>>
		get() = symbolic.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<R> {
		return Getter<R, CType, T>(deconstruct, symbolic.substitute(find, replace), cType)
	}

	override fun simplify(depth: Int): Symbolic<R> {
		val simple = symbolic.simplifyFully()
		if (simple.javaClass == cType.java)
			return deconstruct(simple as CType).simplify(depth - 1)
		else
			return Getter<R, CType, T>(deconstruct, simple, cType)
	}


}


/**
 * Generates a "getter" object for a certain property and constructor class.
 *
 * It works by simplifying a symbolic down to its' constructor,
 * then using one the constructor parameters.
 *
 * @param prop Property of the constructor for which to create the getter
 * @param symbolic The symbolic from which to try to get the property.
 * @param cType Reference to the constructor class
 *
 * @param R "Return type" of the getter
 * @param CType The type of the constructor
 * @param T The symbolic type that the constructor wraps.
 *
 * Warning: implementation is a horrific hack through reflection.
 * Anyone any tips on how to improve this?
 */
inline fun <R : Any, reified CType : Symbolic<T>, T : Any> getter(
		noinline deconstruct: (CType) -> Symbolic<R>,
		symbolic: Symbolic<T>): Symbolic<R> {
	return Getter(deconstruct, symbolic, CType::class)
}