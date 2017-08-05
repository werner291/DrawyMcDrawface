package symbolic

typealias SymIterable<T> = Symbolic<Iterable<T>>

val <T>SymIterable<T>.size
	get() = Getter<Iterable<T>, Int>("size", this, { it.count() })

fun <T : Any> SymIterable<T>.first() =
		Getter<Iterable<T>, T>("size", this, { it.first() })

data class CSymIterable<T : Any>(val items: Iterable<Symbolic<T>>) : SymIterable<T> {
	override fun eval(): Iterable<T> {
		return items.map { it.eval() }
	}

	override val variables: Set<Variable<out Any>>
		get() = items.map { it.variables }
				.fold(emptySet(), { a, b -> a + b })

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<Iterable<T>> {
		return CSymIterable<T>(items.map { it.substitute(find, replace) })
	}
}

data class MappedIterable<T : Any, R : Any>(val iterable: SymIterable<T>,
											val op: Symbolic<R>,
											val varToReplace: Variable<T>) :
		BinarySymbolicOp<Iterable<T>, R, Iterable<R>> {

	override fun createOp(argA: SymIterable<T>, argB: Symbolic<R>) =
			MappedIterable<T, R>(argA, argB, varToReplace)


	override val argA = iterable
	override val argB = op

	override fun eval() = iterable.eval().map { TODO("Figure this out.") }
}


fun <T : Any, R : Any, Op : Symbolic<R>> SymIterable<T>.map(op: Op,
															varToReplace: Variable<T> = Variable<T>("it")): Symbolic<Iterable<R>> {
	return MappedIterable<T, R>(this, op, varToReplace)
}

data class SymUnaryOp<A : Any, R : Any>(val a: Symbolic<A>,
										val op: (A) -> R) : Symbolic<R> {
	override fun eval(): R {
		return op(a.eval())
	}

	override val variables: Set<Variable<out Any>>
		get() = a.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Symbolic<R> {
		return SymUnaryOp<A, R>(a.substitute(find, replace), op)
	}
}

data class SymBinaryOp<A : Any, B : Any, R : Any>(val a: Symbolic<A>,
												  val b: Symbolic<B>,
												  val op: (A, B) -> R) : Symbolic<R> {
	override fun eval(): R {
		return op(a.eval(), b.eval())
	}

	override val variables: Set<Variable<out Any>>
		get() = a.variables + b.variables

	override fun <V : Any> substituteInside(find: Symbolic<V>,
											replace: Symbolic<V>): Symbolic<R> {
		return SymBinaryOp<A, B, R>(a.substitute(find, replace), b.substitute(find, replace), op)
	}
}

fun <T : Any> SymIterable<out T>.concat(other: SymIterable<out T>) =
		SymBinaryOp(this, other, { a, b -> a + b })

fun <T : Any> SymIterable<out T>.append(other: Symbolic<out T>) =
		SymBinaryOp(this, other, { a, b -> a + b })

fun <T : Any> SymIterable<out T>.pairwise() =
		SymUnaryOp(this, { it.pairwise() })

fun <T : Any> SymIterable<Iterable<T>>.flatten(): SymIterable<T> =
		SymUnaryOp(this, { it.flatten() })

fun <T : Any> SymIterable<out T>.drop(n: Int) =
		SymUnaryOp(this, { it.drop(n) })

fun <T : Any> symListOf(vararg t: Symbolic<T>) = CSymIterable(t.toList())