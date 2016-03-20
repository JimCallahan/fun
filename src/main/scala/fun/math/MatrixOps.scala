/*
 * Copyright 2011-2016 James Michael Callahan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fun.math

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.DoubleBuffer
import java.nio.FloatBuffer

/** An immutable square matrix. */
trait MatrixOps[@specialized(Double) Elem, V <: Vec, Repr <: Matrix] {
  /** The number of rows/columns. **/
  def size: Int

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean

  /** The component-wise comparison of whether the given matrix is within a given
    * epsilon of this matrix. */
  def equiv(that: Repr, epsilon: Double): Boolean

  /** The component-wise comparison of whether the given matrix is within a type
    * specific epsilon of this matrix. */
  def equiv(that: Repr): Boolean

  /** Whether any of the matrix components are Not-a-Number (NaN). */
  def isNaN: Boolean

  /** Post-multiplying a column vector by the matrix. */
  def xform(v: V): V

  /** Post-multiplying a column vector by the matrix. */
  def *(v: V): V

  /** Concatenate (multiply) a matrix (on the right) with this matrix. */
  def concat(that: Repr): Repr

  /** Concatenate (multiply) a matrix (on the right) with this matrix. */
  def *(that: Repr): Repr

  /** The addition of a scalar value to all components of this matrix. */
  def +(scalar: Elem): Repr

  /** The subtraction of a scalar value from all components of this matrix. */
  def -(scalar: Elem): Repr

  /** The product of a scalar value with all components of this matrix. */
  def *(scalar: Elem): Repr

  /** The quotient of dividing all components of this matrix by a scalar value. */
  def /(scalar: Elem): Repr

  /** Find the transpose of this matrix. */
  def transpose: Repr

  /** Find the minor of the given cell (column, row) of this matrix.
    *
    * The minor is the determinant of the (N-1)x(N-1) matrix which remains when the row and column of the
    * given cell are removed from the original NxN matrix. */
  def minor(col: Int, row: Int): Elem

  /** Find the matrix of minors of this matrix. */
  def minors: Repr

  /** Find the cofactor of the given cell (column, row) of this matrix.
    *
    * The cofactor of a cell is the minor in which the sign of the result is determined by
    * whether the sum of the column and row indices in the original matrix is even (unchanged)
    * or odd (flipped). */
  def cofactor(col: Int, row: Int): Elem

  /** Find the matrix of cofactors of this matrix. */
  def cofactors: Repr

  /** Find the adjoint of this matrix.
    *
    * The adjoint is the transpose of the cofactors matrix. */
  def adjoint: Repr

  /** Find the determinant of this matrix. */
  def determinant: Elem

  /** Whether the matrix has an inverse (is non-singular). */
  def isInvertible: Boolean

  /** Find the inverse (if possible) of this matrix. */
  def inverse: Option[Repr]

  /** Tests whether the given predicate holds true for all components of this matrix. */
  def forall(p: (Elem) => Boolean): Boolean

  /** Tests whether the given predicate holds true for all of the corresponding components
    * of this and the given matrix. */
  def forall(that: Repr)(p: (Elem, Elem) => Boolean): Boolean

  /** Tests whether the given predicate holds true for any components of this matrix. */
  def forany(p: (Elem) => Boolean): Boolean

  /** Tests whether the given predicate holds true for any of the corresponding components
    * of this and the given matrix. */
  def forany(that: Repr)(p: (Elem, Elem) => Boolean): Boolean

  /** Applies a function to all components of this matrix.
    *
    * @param f The function that is applied for its side-effect to every component.
    */
  def foreach(f: (Elem) => Unit): Unit

  /** Builds a new matrix by applying a function to each component of this matrix. */
  def map(f: (Elem) => Elem): Repr

  /** Convert to a nested list */
  def toList: List[List[Elem]]

  /** Convert to a nested array. */
  def toArray: Array[Array[Elem]]

  /** Add the component values (as Floats) of the matrix starting at the current position to given
    * native array. */
  def putNative(buf: FloatBuffer)

  /** Add the component values (as Floats) of the matrix starting at the current position to given
    * native array. */
  def >>>(buf: FloatBuffer)

  /** Apply the given function to a native array filled with the values (as Floats) from this matrix. */
  def useNativeFloats(p: FloatBuffer => Unit) {
    val buf = ByteBuffer.allocateDirect((size * size) << 2).order(ByteOrder.nativeOrder).asFloatBuffer
    this >>> buf
    buf.rewind
    p(buf)
    buf.clear
  }

  /** Add the component values of the matrix starting at the current position to given native array. */
  def putNative(buf: DoubleBuffer)

  /** Add the component values of the matrix starting at the current position to given native array. */
  def >>>(buf: DoubleBuffer)

  /** Apply the given function to a native array filled with the values from this matrix. */
  def useNativeDoubles(p: DoubleBuffer => Unit) {
    val buf = ByteBuffer.allocateDirect((size * size) << 3).order(ByteOrder.nativeOrder).asDoubleBuffer
    this >>> buf
    buf.rewind
    p(buf)
    buf.clear
  }
}
