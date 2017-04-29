/*
 * Copyright (c) 2017 Werner Kroneman
 *
 * This file is part of DrawyMcDrawface.
 *
 * DrawyMcDrawface is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * DrawyMcDrawface is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with DrawyMcDrawface.  If not, see <http://www.gnu.org/licenses/>.
 */

package nl.wernerkroneman.Drawy.Modelling

import kotlin.reflect.KProperty

/**
 * We do not use Double directly since Scalar is more a
 * specification of a number rather than a number.
 *
 * It may not be deterministic, and maintains the structures
 * used to create it.
 */
abstract class Scalar {
    abstract val value: Double

    operator fun times(other: Scalar): Scalar {
        return MulScalar(this, other)
    }
}

/**
 * Scalar directly representing a specific number.
 */
class AbsoluteScalar(override val value: Double) : Scalar()

/**
 * Product of two scalars.
 */
class MulScalar(val a: Scalar, val b: Scalar) : Scalar() {
    override val value: Double
        get() = a.value * b.value
}

enum class LengthUnit {
    METER
}

/**
 * Represents a 1-dimensional lenggth with a unit, METER by default.
 */
class Length(val scalar: Scalar, val unit: LengthUnit = LengthUnit.METER) {

    fun convertTo(otherUnit: LengthUnit): Length {
        if (this.unit == LengthUnit.METER && otherUnit == LengthUnit.METER)
            return this
        else
            TODO("Meter is best unit. Use meters.")
    }

    operator fun times(other: Scalar): Length {
        return Length(scalar * other, unit)
    }

    operator fun compareTo(length: Length): Int {
        return scalar.value.compareTo(length.convertTo(unit).scalar.value)
    }
}

// TODO: This is too simple. Need to do parameters properly
abstract class Size {

    abstract var x: Length
    abstract var y: Length
    abstract var z: Length

    abstract operator fun times(scalar: Scalar): Size

}

class AbsoluteSize(override var x: Length,
                   override var y: Length,
                   override var z: Length) : Size() {

    override fun times(scalar: Scalar): Size {
        return AbsoluteSize(x * scalar, y * scalar, z * scalar)
    }

    constructor(all: Length) : this(all, all, all)
}

class DerivedSize(val model: ModelSpecification) : Size() {
    override var x: Length by DelegatedUntilSet({ model.size.x })
    override var y: Length by DelegatedUntilSet({ model.size.y })
    override var z: Length by DelegatedUntilSet({ model.size.z })

    override fun times(scalar: Scalar): Size {
        return AbsoluteSize(x * scalar, y * scalar, z * scalar)
    }
}

val DEFAULT_SIZE = AbsoluteSize(Length(AbsoluteScalar(1.0), LengthUnit.METER))

// Thanks to http://stackoverflow.com/questions/43437861/how-to-implement-a-property-that-draws-from-a-certain-source-until-it-is-set-dir
class DelegatedUntilSet<T>(private val getter: () -> T) {
    private var isInitialised = false
    private var value: T? = null

    operator fun getValue(thisRef: Any?, property: KProperty<*>): T =
            if (isInitialised)
                (value as T)
            else getter()

    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
        this.value = value
        isInitialised = true
    }
}