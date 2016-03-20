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
//                Matrix (i, j)
//          j=0      j=1      j=2      j=3
//  i=0 | basisX.x basisY.x basisZ.x basisW.x |
//  i=1 | basisX.y basisY.y basisZ.y basisW.y |
//  i=2 | basisX.z basisY.z basisZ.z basisW.z |
//  i=3 | basisX.w basisY.w basisZ.w basisW.w |
//--------------------------------------------------------------------------------------------------

class Matrix44d private (val basisX: Vec4d, val basisY: Vec4d, val basisZ: Vec4d, val basisW: Vec4d)
    extends Matrix22[Double, Vec4d]
    with MatrixOps[Double, Vec4d, Matrix44d] {

  def size: Int = 4

  /** Compares this position to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Matrix44d =>
        (that canEqual this) &&
        (basisX == that.basisX) &&
        (basisY == that.basisY) &&
        (basisZ == that.basisZ) &&
        (basisW == that.basisW)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Matrix44d => true
      case _               => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 51 * (47 * (43 * (41 + basisX.##) + basisY.##) + basisZ.##) + basisW.##

  def apply(i: Int, j: Int): Double =
    try {
      j match {
        case 0 => basisX(i)
        case 1 => basisY(i)
        case 2 => basisZ(i)
        case 3 => basisW(i)
        case _ => throw new IllegalArgumentException
      }
    }
    catch {
      case _: IllegalArgumentException =>
        throw new IllegalArgumentException("Invalid index (" + i + ", " + j + ")!")
    }

  def xform(v: Vec4d): Vec4d = basisX * v.x + basisY * v.y + basisZ * v.z + basisW * v.w
  def *(v: Vec4d): Vec4d = xform(v)

  def concat(that: Matrix44d): Matrix44d =
    Matrix44d(xform(that.basisX), xform(that.basisY), xform(that.basisZ), xform(that.basisW))
  def *(that: Matrix44d): Matrix44d = concat(that)

  def +(scalar: Double): Matrix44d =
    Matrix44d(basisX + scalar, basisY + scalar, basisZ + scalar, basisW + scalar)
  def -(scalar: Double): Matrix44d =
    Matrix44d(basisX - scalar, basisY - scalar, basisZ - scalar, basisW - scalar)
  def *(scalar: Double): Matrix44d =
    Matrix44d(basisX * scalar, basisY * scalar, basisZ * scalar, basisW * scalar)
  def /(scalar: Double): Matrix44d =
    Matrix44d(basisX / scalar, basisY / scalar, basisZ / scalar, basisW / scalar)

  def transpose: Matrix44d =
    Matrix44d(
      Vec4d(basisX.x, basisY.x, basisZ.x, basisZ.x),
      Vec4d(basisX.y, basisY.y, basisZ.y, basisZ.y),
      Vec4d(basisX.z, basisY.z, basisZ.z, basisZ.z),
      Vec4d(basisX.w, basisY.w, basisZ.w, basisZ.w))

  /** Find the 3x3 sub-matrix obtained by deleting the given column and row. */
  def submatrix(col: Int, row: Int): Matrix33d =
    (col, row) match {
      case (0, 0) => Matrix33d(
        Vec3d(basisY.y, basisY.z, basisY.w),
        Vec3d(basisZ.y, basisZ.z, basisZ.w),
        Vec3d(basisW.y, basisW.z, basisW.w))

      case (0, 1) => Matrix33d(
        Vec3d(basisY.x, basisY.z, basisY.w),
        Vec3d(basisZ.x, basisZ.z, basisZ.w),
        Vec3d(basisW.x, basisW.z, basisW.w))

      case (0, 2) => Matrix33d(
        Vec3d(basisY.x, basisY.y, basisY.w),
        Vec3d(basisZ.x, basisZ.y, basisZ.w),
        Vec3d(basisW.x, basisW.y, basisW.w))

      case (0, 3) => Matrix33d(
        Vec3d(basisY.x, basisY.y, basisY.z),
        Vec3d(basisZ.x, basisZ.y, basisZ.z),
        Vec3d(basisW.x, basisW.y, basisW.z))

      case (1, 0) => Matrix33d(
        Vec3d(basisX.y, basisX.z, basisX.w),
        Vec3d(basisZ.y, basisZ.z, basisZ.w),
        Vec3d(basisW.y, basisW.z, basisW.w))

      case (1, 1) => Matrix33d(
        Vec3d(basisX.x, basisX.z, basisX.w),
        Vec3d(basisZ.x, basisZ.z, basisZ.w),
        Vec3d(basisW.x, basisW.z, basisW.w))

      case (1, 2) => Matrix33d(
        Vec3d(basisX.x, basisX.y, basisX.w),
        Vec3d(basisZ.x, basisZ.y, basisZ.w),
        Vec3d(basisW.x, basisW.y, basisW.w))

      case (1, 3) => Matrix33d(
        Vec3d(basisX.x, basisX.y, basisX.z),
        Vec3d(basisZ.x, basisZ.y, basisZ.z),
        Vec3d(basisW.x, basisW.y, basisW.z))

      case (2, 0) => Matrix33d(
        Vec3d(basisX.y, basisX.z, basisX.w),
        Vec3d(basisY.y, basisY.z, basisY.w),
        Vec3d(basisW.y, basisW.z, basisW.w))

      case (2, 1) => Matrix33d(
        Vec3d(basisX.x, basisX.z, basisX.w),
        Vec3d(basisY.x, basisY.z, basisY.w),
        Vec3d(basisW.x, basisW.z, basisW.w))

      case (2, 2) => Matrix33d(
        Vec3d(basisX.x, basisX.y, basisX.w),
        Vec3d(basisY.x, basisY.y, basisY.w),
        Vec3d(basisW.x, basisW.y, basisW.w))

      case (2, 3) => Matrix33d(
        Vec3d(basisX.x, basisX.y, basisX.z),
        Vec3d(basisY.x, basisY.y, basisY.z),
        Vec3d(basisW.x, basisW.y, basisW.z))

      case (3, 0) => Matrix33d(
        Vec3d(basisX.y, basisX.z, basisX.w),
        Vec3d(basisY.y, basisY.z, basisY.w),
        Vec3d(basisZ.y, basisZ.z, basisZ.w))

      case (3, 1) => Matrix33d(
        Vec3d(basisX.x, basisX.z, basisX.w),
        Vec3d(basisY.x, basisY.z, basisY.w),
        Vec3d(basisZ.x, basisZ.z, basisZ.w))

      case (3, 2) => Matrix33d(
        Vec3d(basisX.x, basisX.y, basisX.w),
        Vec3d(basisY.x, basisY.y, basisY.w),
        Vec3d(basisZ.x, basisZ.y, basisZ.w))

      case (3, 3) => Matrix33d(
        Vec3d(basisX.x, basisX.y, basisX.z),
        Vec3d(basisY.x, basisY.y, basisY.z),
        Vec3d(basisZ.x, basisZ.y, basisZ.z))

      case _ => throw new IllegalArgumentException("Invalid column (" + col + ") or row (" + row + ")!")
    }

  def minor(col: Int, row: Int): Double =
    submatrix(col, row).determinant

  def minors: Matrix44d =
    Matrix44d(Vec4d(minor(0, 0), minor(0, 1), minor(0, 2), minor(0, 3)),
      Vec4d(minor(1, 0), minor(1, 1), minor(1, 2), minor(1, 3)),
      Vec4d(minor(2, 0), minor(2, 1), minor(2, 2), minor(2, 3)),
      Vec4d(minor(3, 0), minor(3, 1), minor(3, 2), minor(3, 3)))

  def cofactor(col: Int, row: Int): Double = {
    val mm = minor(col, row)
    if ((col + row) % 2 == 0) mm else -mm
  }

  def cofactors: Matrix44d = {
    val even = Vec4d(1.0, -1.0, 1.0, -1.0)
    val odd = even * -1.0
    val mm = minors
    Matrix44d(mm.basisX * even, mm.basisY * odd, mm.basisZ * even, mm.basisW * odd)
  }

  def adjoint: Matrix44d = cofactors.transpose
  def determinant: Double =
    basisX.x * cofactor(0, 0) + basisX.y * cofactor(0, 1) +
  basisX.z * cofactor(0, 2) + basisX.w * cofactor(0, 3)

  def isInvertible: Boolean = !Scalar.equiv(determinant, 0.0)

  def inverse: Option[Matrix44d] = {
    val det = determinant
    if (Scalar.equiv(det, 0.0)) None
    else Some(adjoint / det)
  }

  def forall(p: (Double) => Boolean): Boolean =
    basisX.forall(p) && basisY.forall(p) && basisZ.forall(p) && basisW.forall(p)

  def forall(that: Matrix44d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forall(that.basisX)(p) && basisY.forall(that.basisY)(p) &&
  basisZ.forall(that.basisZ)(p) && basisW.forall(that.basisW)(p)

  def equiv(that: Matrix44d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Matrix44d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = basisX.isNaN || basisY.isNaN || basisZ.isNaN || basisW.isNaN

  def forany(p: (Double) => Boolean): Boolean =
    basisX.forany(p) && basisY.forany(p) && basisZ.forany(p) && basisW.forany(p)

  def forany(that: Matrix44d)(p: (Double, Double) => Boolean): Boolean =
    basisX.forany(that.basisX)(p) && basisY.forany(that.basisY)(p) &&
  basisZ.forany(that.basisZ)(p) && basisW.forany(that.basisW)(p)

  def foreach(f: (Double) => Unit): Unit = {
    basisX.foreach(f); basisY.foreach(f); basisZ.foreach(f); basisW.foreach(f)
  }

  def map(f: (Double) => Double): Matrix44d =
    Matrix44d(basisX.map(f), basisY.map(f), basisZ.map(f), basisW.map(f))

  /** Convert to a string representation. */
  override def toString() = "Matrix44d(" + basisX + ", " + basisY + ", " + basisZ + ", " + basisW + ")"

  def toList: List[List[Double]] =
    List(
      List(basisX.x, basisX.y, basisX.z, basisX.w),
      List(basisY.x, basisY.y, basisY.z, basisY.w),
      List(basisZ.x, basisZ.y, basisZ.z, basisZ.w),
      List(basisW.x, basisW.y, basisW.z, basisW.w))

  def toArray: Array[Array[Double]] =
    Array(
      Array(basisX.x, basisX.y, basisX.z, basisX.w),
      Array(basisY.x, basisY.y, basisY.z, basisY.w),
      Array(basisZ.x, basisZ.y, basisZ.z, basisZ.w),
      Array(basisW.x, basisW.y, basisW.z, basisW.w))

  def putNative(buf: DoubleBuffer) {
    buf.put(basisX.x); buf.put(basisX.y); buf.put(basisX.z); buf.put(basisX.w)
    buf.put(basisY.x); buf.put(basisY.y); buf.put(basisY.z); buf.put(basisY.w)
    buf.put(basisZ.x); buf.put(basisZ.y); buf.put(basisZ.z); buf.put(basisZ.w)
    buf.put(basisW.x); buf.put(basisW.y); buf.put(basisW.z); buf.put(basisW.w)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(basisX.x.toFloat); buf.put(basisX.y.toFloat); buf.put(basisX.z.toFloat); buf.put(basisX.w.toFloat)
    buf.put(basisY.x.toFloat); buf.put(basisY.y.toFloat); buf.put(basisY.z.toFloat); buf.put(basisY.w.toFloat)
    buf.put(basisZ.x.toFloat); buf.put(basisZ.y.toFloat); buf.put(basisZ.z.toFloat); buf.put(basisZ.w.toFloat)
    buf.put(basisW.x.toFloat); buf.put(basisW.y.toFloat); buf.put(basisW.z.toFloat); buf.put(basisW.w.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }
}

object Matrix44d {
  import scala.math.{ cos, sin }

  /** Create an identity matrix. */
  def apply(): Matrix44d =
    new Matrix44d(Vec4d.unitX, Vec4d.unitY, Vec4d.unitZ, Vec4d.unitW)

  /** Create a matrix from the given basis vectors. */
  def apply(basisX: Vec4d, basisY: Vec4d, basisZ: Vec4d, basisW: Vec4d): Matrix44d =
    new Matrix44d(basisX, basisY, basisZ, basisW)

  /** Create an matrix frame from nested lists of elements. */
  def apply(mx: List[List[Double]]): Matrix44d =
    mx match {
      case List(
        List(bxx, bxy, bxz, bxw),
        List(byx, byy, byz, byw),
        List(bzx, bzy, bzz, bzw),
        List(bwx, bwy, bwz, bww)) =>
        new Matrix44d(
          Vec4d(bxx, bxy, bxz, bxw),
          Vec4d(byx, byy, byz, byw),
          Vec4d(bzx, bzy, bzz, bzw),
          Vec4d(bwx, bwy, bwz, bww))
      case _ => throw new IllegalArgumentException(
        "The given nested list of values did not correspond to a legal 4x4 matrix!")
    }

  /** Create an matrix frame from nested arrays. */
  def apply(mx: Array[Array[Double]]): Matrix44d =
    mx match {
      case Array(
        Array(bxx, bxy, bxz, bxw),
        Array(byx, byy, byz, byw),
        Array(bzx, bzy, bzz, bzw),
        Array(bwx, bwy, bwz, bww)) =>
        new Matrix44d(
          Vec4d(bxx, bxy, bxz, bxw),
          Vec4d(byx, byy, byz, byw),
          Vec4d(bzx, bzy, bzz, bzw),
          Vec4d(bwx, bwy, bwz, bww))
      case _ => throw new IllegalArgumentException(
        "The given nested array of values did not correspond to a legal 4x4 matrix!")
    }

  /** Create an arbitrary 4x4 matrix from a native array. */
  def apply(mx: DoubleBuffer): Matrix44d = {
    if (mx.capacity != 16)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
      (mx.get, mx.get, mx.get, mx.get,
        mx.get, mx.get, mx.get, mx.get,
        mx.get, mx.get, mx.get, mx.get,
        mx.get, mx.get, mx.get, mx.get) match {
      case (bxx, bxy, bxz, bxw, byx, byy, byz, byw, bzx, bzy, bzz, bzw, bwx, bwy, bwz, bww) =>
        new Matrix44d(
          Vec4d(bxx, bxy, bxz, bxw),
          Vec4d(byx, byy, byz, byw),
          Vec4d(bzx, bzy, bzz, bzw),
          Vec4d(bwx, bwy, bwz, bww))
        throw new IllegalArgumentException(
          "The given native array of values did not correspond to a legal 3x3 matrix!")
    }
  }

  /** Create an arbitrary 3x3 matrix from a native array. */
  def apply(mx: FloatBuffer): Matrix44d = {
    if (mx.capacity != 16)
      throw new IllegalArgumentException(
        "The given native array did not contain (9) values!")
    mx.rewind
      (mx.get.toDouble, mx.get.toDouble, mx.get.toDouble, mx.get.toDouble,
        mx.get.toDouble, mx.get.toDouble, mx.get.toDouble, mx.get.toDouble,
        mx.get.toDouble, mx.get.toDouble, mx.get.toDouble, mx.get.toDouble,
        mx.get.toDouble, mx.get.toDouble, mx.get.toDouble, mx.get.toDouble) match {
      case (bxx, bxy, bxz, bxw, byx, byy, byz, byw, bzx, bzy, bzz, bzw, bwx, bwy, bwz, bww) =>
        new Matrix44d(
          Vec4d(bxx, bxy, bxz, bxw),
          Vec4d(byx, byy, byz, byw),
          Vec4d(bzx, bzy, bzz, bzw),
          Vec4d(bwx, bwy, bwz, bww))
        throw new IllegalArgumentException(
          "The given native array of values did not correspond to a legal 3x3 matrix!")
    }
  }

  def unapply(m: Matrix44d): Some[(Vec4d, Vec4d, Vec4d, Vec4d)] =
    Some((m.basisX, m.basisY, m.basisZ, m.basisW))

  /** Create a new diagonal matrix.
    *
    * | v.x 0.0 0.0 0.0 |
    * | 0.0 v.y 0.0 0.0 |
    * | 0.0 0.0 v.z 0.0 |
    * | 0.0 0.0 0.0 v.w |
    */
  def diagonal(v: Vec4d): Matrix44d =
    Matrix44d(Vec4d.unitX * v.x, Vec4d.unitY * v.y, Vec4d.unitZ * v.z, Vec4d.unitW * v.w)

  /** Create a new diagonal matrix.
    *
    * |  x  0.0 0.0 0.0 |
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 |
    * | 0.0 0.0 0.0  w  |
    */
  def diagonal(x: Double, y: Double, z: Double, w: Double): Matrix44d =
    diagonal(Vec4d(x, y, z, w))

  /** Create a new uniform scaling matrix.
    *
    * |  s  0.0 0.0 0.0 |
    * | 0.0  s  0.0 0.0 |
    * | 0.0 0.0  s  0.0 |
    * | 0.0 0.0 0.0 1.0 |
    */
  def scale(s: Double): Matrix44d =
    diagonal(Vec4d(s, s, s, 1.0))

  /** Create a new non-uniform scaling matrix.
    *
    * |  x  0.0 0.0 0.0 |
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 |
    * | 0.0 0.0 0.0 1.0 |
    */
  def scale(v: Vec3d): Matrix44d =
    diagonal(v.toVec4d + Vec4d.unitW)

  /** Create a new non-uniform scaling matrix.
    *
    * |  x  0.0 0.0 0.0 |
    * | 0.0  y  0.0 0.0 |
    * | 0.0 0.0  z  0.0 |
    * | 0.0 0.0 0.0 1.0 |
    */
  def scale(x: Double, y: Double, z: Double): Matrix44d =
    diagonal(Vec4d(x, y, z, 1.0))

  /** Create a new translation matrix.
    *
    * | 1.0 0.0 0.0  x  |
    * | 0.0 1.0 0.0  y  |
    * | 0.0 0.0 1.0  z  |
    * | 0.0 0.0 0.0 1.0 |
    */
  def translate(v: Vec3d): Matrix44d =
    Matrix44d(Vec4d.unitX, Vec4d.unitY, Vec4d.unitZ, v.toVec4d + Vec4d.unitW)

  /** Create a new translation matrix.
    *
    * | 1.0 0.0 0.0  x  |
    * | 0.0 1.0 0.0  y  |
    * | 0.0 0.0 1.0  z  |
    * | 0.0 0.0 0.0 1.0 |
    */
  def translate(x: Double, y: Double, z: Double): Matrix44d =
    Matrix44d(Vec4d.unitX, Vec4d.unitY, Vec4d.unitZ, Vec4d(x, y, z, 1.0))

  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about an arbitrary axis.
    */
  def rotate(axis: Vec3d, angle: Double): Matrix44d = {
    val s = sin(angle)
    val c = cos(angle)
    val omc = 1.0 - c
    Matrix44d(
      Vec4d(omc * axis.x * axis.x + c,
        omc * axis.x * axis.y + s * axis.z,
        omc * axis.z * axis.x - s * axis.y,
        0.0),
      Vec4d(omc * axis.x * axis.y - s * axis.z,
        omc * axis.y * axis.y + c,
        omc * axis.y * axis.z + s * axis.x,
        0.0),
      Vec4d(omc * axis.z * axis.x + s * axis.y,
        omc * axis.y * axis.z - s * axis.x,
        omc * axis.z * axis.z + c,
        0.0),
      Vec4d.unitW)
  }

  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about the X axis.
    *
    * | 1.0 0.0  0.0 0.0 |
    * | 0.0 cos -sin 0.0 |
    * | 0.0 sin  cos 0.0 |
    * | 0.0 0.0  0.0 1.0 |
    */
  def rotateX(angle: Double): Matrix44d = {
    val s = sin(angle)
    val c = cos(angle)
    Matrix44d(Vec4d.unitX, Vec4d(0.0, c, s, 0.0), Vec4d(0.0, -s, c, 0.0), Vec4d.unitW)
  }

  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about the Y axis.
    *
    * |  cos 0.0 sin 0.0 |
    * |  0.0 1.0 0.0 0.0 |
    * | -sin 0.0 cos 0.0 |
    * |  0.0 0.0 0.0 1.0 |
    */
  def rotateY(angle: Double): Matrix44d = {
    val s = sin(angle)
    val c = cos(angle)
    Matrix44d(Vec4d(c, 0.0, -s, 0.0), Vec4d.unitY, Vec4d(s, 0.0, c, 0.0), Vec4d.unitW)
  }

  /** Create a new rotation matrix described by a counter-clockwise rotation of
    * the given number of radians about the Z axis.
    *
    * | cos -sin 0.0 0.0 |
    * | sin  cos 0.0 0.0 |
    * | 0.0  0.0 1.0 0.0 |
    * | 0.0  0.0 0.0 1.0 |
    */
  def rotateZ(angle: Double): Matrix44d = {
    val s = sin(angle)
    val c = cos(angle)
    Matrix44d(Vec4d(c, s, 0.0, 0.0), Vec4d(-s, c, 0.0, 0.0), Vec4d.unitZ, Vec4d.unitW)
  }

  /** The number of rows/columns. **/
  val size = Matrix44d().size
}
