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
//              Matrix (i, j)
//          j=0      j=1      j=2
//  i=0 | basisX.x basisY.x basisZ.x |
//  i=1 | basisX.y basisY.y basisZ.y |
//  i=2 | basisX.z basisY.z basisZ.z |
//--------------------------------------------------------------------------------------------------

class Matrix33d private (val basisX: Vec3d, val basisY: Vec3d, val basisZ: Vec3d)
    extends Matrix22[Double, Vec3d]
    with MatrixOps[Double, Vec3d, Matrix33d] {

  def size: Int = 3

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Matrix33d =>
        (that canEqual this) &&
          (basisX == that.basisX) && (basisY == that.basisY) && (basisZ == that.basisZ)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Matrix33d => true
      case _               => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 47 * (43 * (41 + basisX.##) + basisY.##) + basisZ.##

  def apply(i: Int, j: Int): Double =
    try {
      j match {
        case 0 => basisX(i)
        case 1 => basisY(i)
        case 2 => basisZ(i)
        case _ => throw new IllegalArgumentException
      }
    }
    catch {
      case _: IllegalArgumentException =>
        throw new IllegalArgumentException("Invalid index (" + i + ", " + j + ")!")
    }

  def xform(v: Vec3d): Vec3d = basisX * v.x + basisY * v.y + basisZ * v.z
  def *(v: Vec3d): Vec3d = xform(v)

  def concat(that: Matrix33d): Matrix33d =
    Matrix33d(xform(that.basisX), xform(that.basisY), xform(that.basisZ))
  def *(that: Matrix33d): Matrix33d = concat(that)

  def +(scalar: Double): Matrix33d = Matrix33d(basisX + scalar, basisY + scalar, basisZ + scalar)
  def -(scalar: Double): Matrix33d = Matrix33d(basisX - scalar, basisY - scalar, basisZ - scalar)
  def *(scalar: Double): Matrix33d = Matrix33d(basisX * scalar, basisY * scalar, basisZ * scalar)
  def /(scalar: Double): Matrix33d = Matrix33d(basisX / scalar, basisY / scalar, basisZ / scalar)

  def transpose: Matrix33d =
    Matrix33d(
      Vec3d(basisX.x, basisY.x, basisZ.x),
      Vec3d(basisX.y, basisY.y, basisZ.y),
      Vec3d(basisX.z, basisY.z, basisZ.z))

  /** Find the 2x2 sub-matrix obtained by deleting the given column and row. */
  def submatrix(col: Int, row: Int): Matrix22d =
    (col, row) match {
      case (0, 0) => Matrix22d(Vec2d(basisY.y, basisY.z), Vec2d(basisZ.y, basisZ.z))
      case (0, 1) => Matrix22d(Vec2d(basisY.x, basisY.z), Vec2d(basisZ.x, basisZ.z))
      case (0, 2) => Matrix22d(Vec2d(basisY.x, basisY.y), Vec2d(basisZ.x, basisZ.y))
      case (1, 0) => Matrix22d(Vec2d(basisX.y, basisX.z), Vec2d(basisZ.y, basisZ.z))
      case (1, 1) => Matrix22d(Vec2d(basisX.x, basisX.z), Vec2d(basisZ.x, basisZ.z))
      case (1, 2) => Matrix22d(Vec2d(basisX.x, basisX.y), Vec2d(basisZ.x, basisZ.y))
      case (2, 0) => Matrix22d(Vec2d(basisX.y, basisX.z), Vec2d(basisY.y, basisY.z))
      case (2, 1) => Matrix22d(Vec2d(basisX.x, basisX.z), Vec2d(basisY.x, basisY.z))
      case (2, 2) => Matrix22d(Vec2d(basisX.x, basisX.y), Vec2d(basisY.x, basisY.y))
      case _      => throw new IllegalArgumentException("Invalid column (" + col + ") or row (" + row + ")!")
    }

  def minor(col: Int, row: Int): Double =
    submatrix(col, row).determinant

  def minors: Matrix33d =
    Matrix33d(Vec3d(minor(0, 0), minor(0, 1), minor(0, 2)),
      Vec3d(minor(1, 0), minor(1, 1), minor(1, 2)),
      Vec3d(minor(2, 0), minor(2, 1), minor(2, 2)))

  def cofactor(col: Int, row: Int): Double = {
    val mm = minor(col, row)
    if ((col + row) % 2 == 0) mm else -mm
  }

  def cofactors: Matrix33d = {
    val even = Vec3d(1.0, -1.0, 1.0)
    val odd = even * -1.0
    val mm = minors
    Matrix33d(mm.basisX * even, mm.basisY * odd, mm.basisZ * even)
  }

  def adjoint: Matrix33d = cofactors.transpose
  def determinant: Double = basisX.x * cofactor(0, 0) + basisX.y * cofactor(0, 1) + basisX.z * cofactor(0, 2)

  def isInvertible: Boolean = !Scalar.equiv(determinant, 0.0)

  def inverse: Option[Matrix33d] = {
    val det = determinant
    if (Scalar.equiv(det, 0.0)) None
    else Some(adjoint / det)
  }

  def forall(p: (Double) => Boolean): Boolean =
    basisX.forall(p) && basisY.forall(p) && basisZ.forall(p)

  def forall(that: Matrix33d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) && basisZ.forall(that.basisZ)(p)

  def equiv(that: Matrix33d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Matrix33d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = basisX.isNaN || basisY.isNaN || basisZ.isNaN

  def forany(p: (Double) => Boolean): Boolean =
    basisX.forany(p) && basisY.forany(p) && basisZ.forany(p)

  def forany(that: Matrix33d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p) && basisZ.forany(that.basisZ)(p)

  def foreach(f: (Double) => Unit): Unit = {
    basisX.foreach(f); basisY.foreach(f); basisZ.foreach(f)
  }

  def map(f: (Double) => Double): Matrix33d =
    Matrix33d(basisX.map(f), basisY.map(f), basisZ.map(f))

  /** Convert to a string representation. */
  override def toString() = "Matrix33d(" + basisX + ", " + basisY + ", " + basisZ + ")"

  def toList: List[List[Double]] =
    List(
      List(basisX.x, basisX.y, basisX.z),
      List(basisY.x, basisY.y, basisY.z),
      List(basisZ.x, basisZ.y, basisZ.z))

  def toArray: Array[Array[Double]] =
    Array(
      Array(basisX.x, basisX.y, basisX.z),
      Array(basisY.x, basisY.y, basisY.z),
      Array(basisZ.x, basisZ.y, basisZ.z))

  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y); buf.put(basisX.z)
    buf.put(basisY.x); buf.put(basisY.y); buf.put(basisY.z)
    buf.put(basisZ.x); buf.put(basisZ.y); buf.put(basisZ.z)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat); buf.put(basisX.z.toFloat)
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat); buf.put(basisY.z.toFloat)
    buf.put(basisZ.x.toFloat); buf.put(basisZ.y.toFloat); buf.put(basisZ.z.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }
}

object Matrix33d {
  /** Create an identity matrix. */
  def apply(): Matrix33d =
    new Matrix33d(Vec3d.unitX, Vec3d.unitY, Vec3d.unitZ)

  /** Create a matrix from the given basis vectors. */
  def apply(basisX: Vec3d, basisY: Vec3d, basisZ: Vec3d): Matrix33d =
    new Matrix33d(basisX, basisY, basisZ)

  /** Create an matrix frame from nested lists of elements. */
  def apply(mx: List[List[Double]]): Matrix33d =
    mx match {
      case List(List(bxx, bxy, bxz), List(byx, byy, byz), List(bzx, bzy, bzz)) =>
        new Matrix33d(Vec3d(bxx, bxy, bxz), Vec3d(byx, byy, byz), Vec3d(bzx, bzy, bzz))
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 3x3 matrix!")
    }

  /** Create an matrix frame from nested arrays. */
  def apply(mx: Array[Array[Double]]): Matrix33d =
    mx match {
      case Array(Array(bxx, bxy, bxz), Array(byx, byy, byz), Array(bzx, bzy, bzz)) =>
        new Matrix33d(Vec3d(bxx, bxy, bxz), Vec3d(byx, byy, byz), Vec3d(bzx, bzy, bzz))
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 3x3 matrix!")
    }

  /** Create an arbitrary 3x3 matrix from a native array. */
  def apply(mx: DoubleBuffer): Matrix33d = {
    if (mx.capacity != 9)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
      (mx.get, mx.get, mx.get, mx.get, mx.get, mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz, byx, byy, byz, bzx, bzy, bzz) =>
        throw new IllegalArgumentException(
          "The given native array of values did not correspond to a legal 3x3 matrix!")
    }
  }

  /** Create an arbitrary 3x3 matrix from a native array. */
  def apply(mx: FloatBuffer): Matrix33d = {
    if (mx.capacity != 9)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
      (mx.get.toDouble, mx.get.toDouble, mx.get.toDouble,
        mx.get.toDouble, mx.get.toDouble, mx.get.toDouble,
        mx.get.toDouble, mx.get.toDouble, mx.get.toDouble) match {
      case (bxx, bxy, bxz, byx, byy, byz, bzx, bzy, bzz) =>
        throw new IllegalArgumentException(
          "The given native array of values did not correspond to a legal 3x3 matrix!")
    }
  }

  def unapply(m: Matrix33d): Some[(Vec3d, Vec3d, Vec3d)] =
    Some((m.basisX, m.basisY, m.basisZ))

  /** Create a new diagonal matrix.
    *
    * | v.x 0.0 0.0 |
    * | 0.0 v.y 0.0 |
    * | 0.0 0.0 v.z |
    */
  def diagonal(v: Vec3d): Matrix33d =
    Matrix33d(Vec3d.unitX * v.x, Vec3d.unitY * v.y, Vec3d.unitZ * v.z)

  /** Create a new diagonal matrix.
    *
    * |  x  0.0 0.0 |
    * | 0.0  y  0.0 |
    * | 0.0 0.0  z  |
    */
  def diagonal(x: Double, y: Double, z: Double): Matrix33d =
    diagonal(Vec3d(x, y, z))

  /** The number of rows/columns. **/
  val size = Matrix33d().size
}
