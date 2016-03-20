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

import scala.annotation.tailrec
import scala.math.sin
import scala.math.cos
import scala.math.acos
import scala.math.sqrt
import scala.util.Random

import java.nio.CharBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.ShortBuffer

/** Quaternions.
  * @see [[http://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation#Comparison_with_other_representations_of_rotations]] */
class Quat private (val a: Double, val b: Double, val c: Double, val d: Double) {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Quat =>
        (that canEqual this) &&
        Scalar.equiv(a, that.a) &&
        Scalar.equiv(b, that.b) &&
        Scalar.equiv(c, that.c) &&
        Scalar.equiv(d, that.d)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Quat => true
      case _          => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 53 * (47 * (43 * (41 + a.##) + b.##) + c.##) + d.##

  /** The component identified by the given index. */
  def apply(i: Int): Double =
    i match {
      case 0 => a
      case 1 => b
      case 2 => c
      case 3 => d
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** A copy of this quaternion in which the component identified by index has been replaced with the given value. */
  def update(i: Int, e: Double): Quat =
    i match {
      case 0 => Quat(e, b, c, d)
      case 1 => Quat(a, e, c, d)
      case 2 => Quat(a, b, e, d)
      case 3 => Quat(a, b, c, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** A copy of this quaternion in which the A-component has been replaced with the given value. */
  def updateA(e: Double): Quat = Quat(e, b, c, d)

  /** A copy of this quaternion in which the B-component has been replaced with the given value. */
  def updateB(e: Double): Quat = Quat(a, e, c, d)

  /** A copy of this quaternion in which the C-component has been replaced with the given value. */
  def updateC(e: Double): Quat = Quat(a, b, e, d)

  /** A copy of this quaternion in which the D-component has been replaced with the given value. */
  def updateD(e: Double): Quat = Quat(a, b, c, e)

  /** The normalized vector along the axis of rotation of this quaternion. */
  def axis: Vec3d = Vec3d(b, c, d).normalized

  /** The angle (in radians) of rotation about the axis of this quaternion. */
  def angle: Double = 2.0 * acos(a)

  /** The magnitude of this quaternion squared (should be one). */
  def magSq: Double = a * a + b * b + c * c + d * d

  /** The magnitude of this quaternion (should be one). */
  def mag: Double = sqrt(magSq)

  /** A renormalized version of this quaternion. */
  def normalized: Quat = {
    val m = mag
    if (Scalar.equiv(m, 0.0)) Quat(0.0)
    else Quat(a / m, b / m, c / m, d / m)
  }

  /** A quaternion representing the opposite rotation around the same axis. */
  def inverted: Quat = Quat(a, -b, -c, -d)

  /** The composition of this with anopther quaterion. */
  def *(that: Quat): Quat =
    Quat(
      a * that.a - b * that.b - c * that.c - d * that.d,
      a * that.b + b * that.a + c * that.d - d * that.c,
      a * that.c - b * that.d + c * that.a + d * that.b,
      a * that.d + b * that.c - c * that.b + d * that.a
    )

  /** Rotate the given vector using this quaternion. */
  def *(that: Vec3d): Vec3d = {
    val r = Vec3d(b, c, d)
    that + (r * 2.0) cross ((r cross that) + (that * a))
  }

  /** Tests whether a predicate holds true for all of the corresponding components
    * of this and another quaternion.
    */
  private def forall(that: Quat)(p: (Double, Double) => Boolean): Boolean =
    p(a, that.a) && p(b, that.b) && p(c, that.c) && p(d, that.d)

  /** Whether another quaternion is within a given epsilon of this vector. */
  def equiv(that: Quat, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))

  /** Whether another quaternion is within a the standard epsilon of this vector. */
  def equiv(that: Quat): Boolean = forall(that)(Scalar.equiv(_, _))

  /** Whether any components are Not-a-Number (NaN). */
  def isNaN: Boolean = a.isNaN || b.isNaN || c.isNaN || d.isNaN

  def lerp(that: Quat, t: Double): Quat = throw new NotImplementedError
  def smoothlerp(that: Quat, t: Double): Quat = throw new NotImplementedError

  /** Convert to a String representation */
  override def toString() = "Quat(%.2f, %.2f, %.2f, %.2f)".format(a, b, c, d)

  def toList: List[Double] = List(a, b, c, d)
  def toArray: Array[Double] = Array(a, b, c, d)
  def toVec4d: Vec4d = Vec4d(a, b, c, d)

  def toMatrix44d: Matrix44d = {
    val aa = a * a
    val bb = b * b
    val cc = c * c
    val dd = d * d
    val ab = 2.0 * a * b
    val ac = 2.0 * a * c
    val ad = 2.0 * a * d
    val bc = 2.0 * b * c
    val bd = 2.0 * b * d
    val cd = 2.0 * c * d
    Matrix44d(
      Vec4d(aa + bb - cc - dd, bc + ad, bd - ac, 0.0),
      Vec4d(bc - ad, aa - bb + cc - dd, cd + ab, 0.0),
      Vec4d(bd + ac, cd - ab, aa - bb - cc + dd, 0.0),
      Vec4d(0.0)
    )
  }

  def putNative(buf: CharBuffer) {
    buf.put(a.toChar); buf.put(b.toChar); buf.put(c.toChar); buf.put(d.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(a.toShort); buf.put(b.toShort); buf.put(c.toShort); buf.put(d.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(a.toInt); buf.put(b.toInt); buf.put(c.toInt); buf.put(d.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(a.toLong); buf.put(b.toLong); buf.put(c.toLong); buf.put(d.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(a.toFloat); buf.put(b.toFloat); buf.put(c.toFloat); buf.put(d.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(a); buf.put(b); buf.put(c); buf.put(d)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Quat {
  def apply(s: Double): Quat = new Quat(s, s, s, s)
  def apply(a: Double, b: Double, c: Double, d: Double): Quat = new Quat(a, b, c, d)
  def apply(v: Vec4d): Quat = new Quat(v.x, v.y, v.z, v.w)

  /** Create from an axis and angle. **/
  def apply(axis: Vec3d, angle: Double): Quat = {
    val v = axis.normalized * sin(angle) * 0.5
    Quat(cos(angle) * 0.5, v.x, v.y, v.z) // .normalized ???
  }

  def apply(ls: List[Double]): Quat =
    ls match {
      case List(a, b, c, d) => Quat(a, b, c, d)
      case List(a, b, c)    => Quat(a, b, c, 0.0)
      case List(a, b)       => Quat(a, b, 0.0, 0.0)
      case List(a)          => Quat(a, 0.0, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 4 components!")
    }

  def apply(ary: Array[Double]): Quat =
    ary match {
      case Array(a, b, c, d) => Quat(a, b, c, d)
      case Array(a, b, c)    => Quat(a, b, c, 0.0)
      case Array(a, b)       => Quat(a, b, 0.0, 0.0)
      case Array(a)          => Quat(a, 0.0, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 4 components!")
    }

  def apply(buf: CharBuffer): Quat =
    Quat(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: ShortBuffer): Quat =
    Quat(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: IntBuffer): Quat =
    Quat(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: LongBuffer): Quat =
    Quat(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: FloatBuffer): Quat =
    Quat(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: DoubleBuffer): Quat =
    Quat(buf.get, buf.get, buf.get, buf.get)

  def unapply(q: Quat): Some[(Double, Double, Double, Double)] = Some((q.a, q.b, q.c, q.d))

  val zero = Quat(0.0)
}
