package nl.wernerkroneman.SymboliK

/**
 * A Symbolic Iterable.
 *
 * This is different than an Iterable<Symbolic<T>>
 * since the iterable itself is symbolic here,
 * yielding for example a variable number of items
 * based on a variable.
 */
typealias SymIterable<T> = Symbolic<Iterable<T>>

val <T : Any>SymIterable<T>.size: Symbolic<Int>
	get() = getter(CSymIterable<T>::size, this)

fun <T : Any> SymIterable<T>.first() =
		getter<T, CSymIterable<T>, Iterable<T>>({ it.items.first() }, this)

/**
 * SymIterable constructor that wraps a non-symbolic Iterable of symbolics.
 */
data class CSymIterable<T : Any>(val items: Iterable<Symbolic<T>>) : SymIterable<T> {

	constructor(vararg items: Symbolic<T>) : this(items.toList())

	override fun eval(): Iterable<T> {
		return items.map { it.eval() }
	}

	override val variables: Set<Variable<out Any>>
		get() = items.map { it.variables }
				.fold(emptySet(), { a, b -> a + b })

	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<Iterable<T>> {
		return CSymIterable<T>(items.map { it.substitute(find, replace) })
	}

	override fun simplify(depth: Int): Symbolic<Iterable<T>> {
		return CSymIterable<T>(items.map { it.simplify(depth - 1) })
	}
}

val <T : Any> CSymIterable<T>.size: Symbolic<Int>
	get() = getter<Int, CSymIterable<T>, Iterable<T>>({
														  IntC(it.items.count())
													  }, this)

/**
 * Represents a mapping of an iterable.
 *
 * @param iterable The symbolic iterable to operate on
 * @param op The symboli cin which to replace the items from the iterable
 * @param varToReplace The variable in [op] to substitute.
 */
data class MappedIterable<T : Any, R : Any>(val iterable: SymIterable<T>,
											val op: Symbolic<R>,
											val varToReplace: Variable<T>) :
		SymIterable<R> {
	override fun <V : Any> substituteInside(find: Symbolic<V>, replace: Symbolic<V>): Symbolic<Iterable<R>> {
		return MappedIterable(iterable.substituteInside(find, replace),
				// TODO: op is not a free variable, is this OK?
							  op.substitute(find, replace),
							  varToReplace)
	}

	override val variables: Set<Variable<out Any>>
		get() = iterable.variables + (op.variables - varToReplace)

	override fun simplify(depth: Int): Symbolic<Iterable<R>> {

		val simpleItr = iterable.simplify(depth - 1)

		return if (simpleItr is CSymIterable)
			CSymIterable(simpleItr.items.map {
				op.substitute(varToReplace, it)
			}).simplify(depth - 1)
		else {
			MappedIterable<T, R>(simpleItr, op.simplify(depth - 1), varToReplace)
		}

	}
}

/**
 * Construct a [MappedIterable]
 */
fun <T : Any, R : Any, Op : Symbolic<R>> SymIterable<T>.map(op: Op,
															varToReplace: Variable<T> = Variable<T>("it")): Symbolic<Iterable<R>> {
	return MappedIterable<T, R>(this, op, varToReplace)
}

/**
 * Concatenate two symbolic iterators
 */
fun <T : Any> SymIterable<out T>.concat(other: SymIterable<out T>): SymIterable<T> =
		SymbolicBinaryOp<Iterable<T>, Iterable<T>, Iterable<T>>(
				this, other, { a, b, depth ->
			if (a is CSymIterable<T> && b is CSymIterable<T>)
				CSymIterable(a.items + b.items).simplify(depth - 1)
			else
				a.concat(b)
		})


/**
 * Append an element to the symbolic iterable
 */
fun <T : Any> SymIterable<out T>.append(other: Symbolic<out T>): SymIterable<T> =
		SymbolicBinaryOp<Iterable<T>, T, Iterable<T>>(
				this, other, { a, b, depth ->
			if (a is CSymIterable<T>)
				CSymIterable(a.items + b).simplify(depth - 1)
			else
				a.append(b)
		})

fun <T : Any> SymIterable<out T>.pairwise(): SymIterable<Pair<T, T>> =
		SymbolicUnaryOp<Iterable<T>, Iterable<Pair<T, T>>>(this, { a, depth ->
			if (a is CSymIterable<T>)
				CSymIterable<Pair<T, T>>(a.items.pairwise { a, b -> CSymPair(a, b) }).simplify(depth - 1)
			else
				a.pairwise()
		})

fun <T : Any> SymIterable<Iterable<T>>.flatten(): SymIterable<T> =
		SymbolicUnaryOp<Iterable<Iterable<T>>, Iterable<T>>(this, { itr, depth ->
			if (itr is CSymIterable<Iterable<T>>)
				itr.items.reduce { acc, symbolic -> acc.concat(symbolic) }.simplify(depth - 1)
			else
				itr.flatten()
		})

fun <T : Any> SymIterable<out T>.drop(n: Int): SymIterable<T> =
		SymbolicUnaryOp<Iterable<T>, Iterable<T>>(this, { itr, depth ->
			if (itr is CSymIterable<T>)
				CSymIterable(itr.items.drop(n)).simplify(depth - 1)
			else
				itr.drop(n)
		})


fun <T : Any> SymIterable<out T>.distinct(): SymIterable<T> =
		SymbolicUnaryOp<Iterable<T>, Iterable<T>>(this, { itr, depth ->
			if (itr is CSymIterable<T>)
				CSymIterable(itr.items.distinct()).simplify(depth - 1)
			else
				itr.distinct()
		})

fun <T : Any> symListOf(vararg t: Symbolic<T>) = CSymIterable(t.toList())