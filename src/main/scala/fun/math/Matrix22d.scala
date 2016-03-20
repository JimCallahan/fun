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

import java.nio.DoubleBuffer
import java.nio.FloatBuffer

//--------------------------------------------------------------------------------------------------
//         Matrix  (i, j)
//          j=0      j=1
//  i=0 | basisX.x basisY.x |
//  i=1 | basisX.y basisY.y |
//--------------------------------------------------------------------------------------------------

class Matrix22d private (val basisX: Vec2d, val basisY: Vec2d)
    extends Matrix22[Double, Vec2d]
    with MatrixOps[Double, Vec2d, Matrix22d] {

  def size: Int = 2

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Matrix22d =>
        (that canEqual this) &&
          (basisX == that.basisX) && (basisY == that.basisY)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Matrix22d => true
      case _               => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 43 * (41 + basisX.##) + basisY.##

  def apply(i: Int, j: Int): Double =
    try {
      j match {
        case 0 => basisX(i)
        case 1 => basisY(i)
        case _ => throw new IllegalArgumentException
      }
    }
    catch {
      case _: IllegalArgumentException =>
        throw new IllegalArgumentException("Invalid index (" + i + ", " + j + ")!")
    }

  def xform(v: Vec2d): Vec2d = basisX * v.x + basisY * v.y
  def *(v: Vec2d): Vec2d = xform(v)

  def concat(that: Matrix22d): Matrix22d =
    Matrix22d(xform(that.basisX), xform(that.basisY))
  def *(that: Matrix22d): Matrix22d = concat(that)

  def +(scalar: Double): Matrix22d = Matrix22d(basisX + scalar, basisY + scalar)
  def -(scalar: Double): Matrix22d = Matrix22d(basisX - scalar, basisY - scalar)
  def *(scalar: Double): Matrix22d = Matrix22d(basisX * scalar, basisY * scalar)
  def /(scalar: Double): Matrix22d = Matrix22d(basisX / scalar, basisY / scalar)

  def transpose: Matrix22d =
    Matrix22d(Vec2d(basisX.x, basisY.x), Vec2d(basisX.y, basisY.y))

  def minor(col: Int, row: Int): Double =
    (col, row) match {
      case (0, 0) => basisY.y
      case (0, 1) => basisY.x
      case (1, 0) => basisX.y
      case (1, 1) => basisX.x
      case _ => throw new IllegalArgumentException(
        "Invalid column (" + col + ") or row (" + row + ")!")
    }

  def minors: Matrix22d =
    Matrix22d(Vec2d(basisY.y, basisY.x), Vec2d(basisX.y, basisX.x))

  def cofactor(col: Int, row: Int): Double = {
    val mm = minor(col, row)
    if ((col + row) % 2 == 0) mm else -mm
  }

  def cofactors: Matrix22d =
    Matrix22d(Vec2d(basisY.y, -basisY.x), Vec2d(-basisX.y, basisX.x))

  def adjoint: Matrix22d = cofactors.transpose
  def determinant: Double = basisX.x * basisY.y - basisY.x * basisX.y

  def isInvertible: Boolean = !Scalar.equiv(determinant, 0.0)

  def inverse: Option[Matrix22d] = {
    val det = determinant
    if (Scalar.equiv(det, 0.0)) None
    else Some(adjoint / det)
  }

  def forall(p: (Double) => Boolean): Boolean =
    basisX.forall(p) && basisY.forall(p)

  def forall(that: Matrix22d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p)

  def equiv(that: Matrix22d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Matrix22d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = basisX.isNaN || basisY.isNaN

  def forany(p: (Double) => Boolean): Boolean =
    basisX.forany(p) && basisY.forany(p)

  def forany(that: Matrix22d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p)

  def foreach(f: (Double) => Unit): Unit = {
    basisX.foreach(f); basisY.foreach(f)
  }

  def map(f: (Double) => Double): Matrix22d =
    Matrix22d(basisX.map(f), basisY.map(f))

  /** Convert to a string representation. */
  override def toString() = "Matrix22d(" + basisX + ", " + basisY + ")"

  def toList: List[List[Double]] =
    List(
      List(basisX.x, basisX.y),
      List(basisY.x, basisY.y))

  def toArray: Array[Array[Double]] =
    Array(
      Array(basisX.x, basisX.y),
      Array(basisY.x, basisY.y))

  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y)
    buf.put(basisY.x); buf.put(basisY.y)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat)
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }
}

object Matrix22d {
  /** Create an identity matrix. */
  def apply(): Matrix22d =
    new Matrix22d(Vec2d.unitX, Vec2d.unitY)

  /** Create a matrix from the given basis vectors. */
  def apply(basisX: Vec2d, basisY: Vec2d): Matrix22d =
    new Matrix22d(basisX, basisY)

  /** Create an matrix frame from nested lists of elements. */
  def apply(mx: List[List[Double]]): Matrix22d =
    mx match {
      case List(List(bxx, bxy), List(byx, byy)) => new Matrix22d(Vec2d(bxx, bxy), Vec2d(byx, byy))
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 2x2 matrix!")
    }

  /** Create an matrix frame from nested arrays. */
  def apply(mx: Array[Array[Double]]): Matrix22d =
    mx match {
      case Array(Array(bxx, bxy), Array(byx, byy)) => new Matrix22d(Vec2d(bxx, bxy), Vec2d(byx, byy))
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 2x2 matrix!")
    }

  /** Create an arbitrary 2x2 matrix from a native array. */
  def apply(mx: DoubleBuffer): Matrix22d = {
    if (mx.capacity != 4)
      throw new IllegalArgumentException(
        "The given native array did not contain (4) values!")
    mx.rewind
      (mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, byx, byy) => new Matrix22d(Vec2d(bxx, bxy), Vec2d(byx, byy))
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 2x2 matrix!")
    }
  }

  /** Create an arbitrary 2x2 matrix from a native array. */
  def apply(mx: FloatBuffer): Matrix22d = {
    if (mx.capacity != 4)
      throw new IllegalArgumentException(
        "The given native array did not contain (4) values!")
    mx.rewind
      (mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, byx, byy) =>
        new Matrix22d(Vec2d(bxx.toDouble, bxy.toDouble), Vec2d(byx.toDouble, byy.toDouble))
      case _ => throw new IllegalArgumentException(
        "The given native array of values did not correspond to a legal 2x2 matrix!")
    }
  }

  def unapply(m: Matrix22d): Some[(Vec2d, Vec2d)] =
    Some((m.basisX, m.basisY))

  /** Create a new diagonal matrix.
    *
    * | v.x 0.0 |
    * | 0.0 v.y |
    */
  def diagonal(v: Vec2d): Matrix22d =
    Matrix22d(Vec2d.unitX * v.x, Vec2d.unitY * v.y)

  /** Create a new diagonal matrix.
    *
    * |  x  0.0 |
    * | 0.0  y  |
    */
  def diagonal(x: Double, y: Double): Matrix22d =
    diagonal(Vec2d(x, y))

  /** The number of rows/columns. **/
  val size = Matrix22d().size
}
