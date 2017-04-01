/*
 * Copyright 2010-2015 JetBrains s.r.o.
 * Adapted 2017 by Guillaume Tournigand and Lionel Fleury
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package kotlin

/**
 * Represents an array (specifically, a Java array when targeting the JVM platform).
 * Array instances can be created using the [arrayOf], [arrayOfNulls] and [emptyArray]
 * standard library functions.
 * See [Kotlin language documentation](http://kotlinlang.org/docs/reference/basic-types.html#arrays)
 * for more information on arrays.
 */
public class Array<O>(vararg val ts: O) {
    /**
     * Creates a new array with the specified [size], where each element is calculated by calling the specified
     * [init] function. The [init] function returns an array element given its index.
     */
    //public inline constructor(size: Int, init: (Int) -> O)

    /**
     * Returns the array element at the specified [index]. This method can be called using the
     * index operator:
     * ```
     * value = arr[index]
     * ```
     */
    public operator fun get(index: Int): O = java.lang.reflect.Array.get(ts, index) as O

    /**
     * Sets the array element at the specified [index] to the specified [value]. This method can
     * be called using the index operator:
     * ```
     * arr[index] = value
     * ```
     */
    public operator fun set(index: Int, value: O): Unit = java.lang.reflect.Array.set(ts, index, value)

    /**
     * Returns the number of elements in the array.
     */
    public val size: Int = java.lang.reflect.Array.getLength(ts)

    /**
     * Creates an iterator for iterating over the elements of the array.
     */
    public operator fun iterator(): Iterator<O> = It(ts, size)

    class It<O>(val ts: Array<O>, val size: Int) : Iterator<O> {
        var index = 0
        override public operator fun next(): O = if (hasNext()) {
            val o = java.lang.reflect.Array.get(ts, index) as O
            index = index + 1
            o
        } else throw java.util.NoSuchElementException()

        override public operator fun hasNext(): Boolean = (index - size + 1) < 0
    }

}
