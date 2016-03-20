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

import java.nio.CharBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.ShortBuffer

class Pos3d private (val x: Double, val y: Double, val z: Double)
    extends Tuple3[Double, Pos3d]
    with Swizzle3[Pos2d, Pos3d]
    with DoubleTupleOps[Pos3d]
    with PosOps[Double, Pos3d, Vec3d]
    with Pos {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Pos3d =>
        (that canEqual this) &&
        Scalar.equiv(x, that.x) &&
        Scalar.equiv(y, that.y) &&
        Scalar.equiv(z, that.z)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Pos3d => true
      case _           => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 47 * (43 * (41 + x.##) + y.##) + z.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Pos3d =
    i match {
      case 0 => Pos3d(e, y, z)
      case 1 => Pos3d(x, e, z)
      case 2 => Pos3d(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Pos3d = Pos3d(e, y, z)
  def updateY(e: Double): Pos3d = Pos3d(x, e, z)
  def updateZ(e: Double): Pos3d = Pos3d(x, y, e)

  def xxx: Pos3d = Pos3d(x, x, x)
  def xxy: Pos3d = Pos3d(x, x, y)
  def xyx: Pos3d = Pos3d(x, y, x)
  def yxx: Pos3d = Pos3d(y, x, x)
  def xxz: Pos3d = Pos3d(x, x, z)
  def xzx: Pos3d = Pos3d(x, z, x)
  def zxx: Pos3d = Pos3d(z, x, x)
  def xyy: Pos3d = Pos3d(x, y, y)
  def yxy: Pos3d = Pos3d(y, x, y)
  def yyx: Pos3d = Pos3d(y, y, x)
  def xyz: Pos3d = Pos3d(x, y, z)
  def xzy: Pos3d = Pos3d(x, z, y)
  def yxz: Pos3d = Pos3d(y, x, z)
  def yzx: Pos3d = Pos3d(y, z, x)
  def zxy: Pos3d = Pos3d(z, x, y)
  def zyx: Pos3d = Pos3d(z, y, x)
  def xzz: Pos3d = Pos3d(x, z, z)
  def zxz: Pos3d = Pos3d(z, x, z)
  def zzx: Pos3d = Pos3d(z, z, x)
  def yyy: Pos3d = Pos3d(y, y, y)
  def yyz: Pos3d = Pos3d(y, y, z)
  def yzy: Pos3d = Pos3d(y, z, y)
  def zyy: Pos3d = Pos3d(z, y, y)
  def yzz: Pos3d = Pos3d(y, z, z)
  def zyz: Pos3d = Pos3d(z, y, z)
  def zzy: Pos3d = Pos3d(z, z, y)
  def zzz: Pos3d = Pos3d(z, z, z)

  def xx: Pos2d = Pos2d(x, x)
  def xy: Pos2d = Pos2d(x, y)
  def yx: Pos2d = Pos2d(y, x)
  def xz: Pos2d = Pos2d(x, z)
  def zx: Pos2d = Pos2d(z, x)
  def yy: Pos2d = Pos2d(y, y)
  def yz: Pos2d = Pos2d(y, z)
  def zy: Pos2d = Pos2d(z, y)
  def zz: Pos2d = Pos2d(z, z)

  def unary_- : Pos3d = Pos3d(-x, -y, -z)
  def -(that: Pos3d): Vec3d = Vec3d(x - that.x, y - that.y, z - that.z)

  def +(that: Vec3d): Pos3d = Pos3d(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3d): Pos3d = Pos3d(x - that.x, y - that.y, z - that.z)
  def *(that: Vec3d): Pos3d = Pos3d(x * that.x, y * that.y, z * that.z)
  def /(that: Vec3d): Pos3d = Pos3d(x / that.x, y / that.y, z / that.z)

  def +(s: Double): Pos3d = Pos3d(x + s, y + s, z + s)
  def -(s: Double): Pos3d = Pos3d(x - s, y - s, z - s)
  def *(s: Double): Pos3d = Pos3d(x * s, y * s, z * s)
  def /(s: Double): Pos3d = Pos3d(x / s, y / s, z / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y) && p(z)
  def forall(that: Pos3d)(p: (Double, Double) => Boolean): Boolean =
    p(x, that.x) && p(y, that.y) && p(z, that.z)

  def equiv(that: Pos3d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Pos3d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = x.isNaN || y.isNaN || z.isNaN

  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y) || p(z)
  def forany(that: Pos3d)(p: (Double, Double) => Boolean): Boolean =
    p(x, that.x) || p(y, that.y) || p(z, that.z)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y); p(z) }

  def map(p: (Double) => Double): Pos3d = Pos3d(p(x), p(y), p(z))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(f(start, x), y), z)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, f(z, start)))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, p(y, z))
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Pos3d)(p: (Double, Double) => Double): Pos3d =
    Pos3d(p(x, that.x), p(y, that.y), p(z, that.z))
  def min(that: Pos3d): Pos3d = compwise(that)(_ min _)
  def max(that: Pos3d): Pos3d = compwise(that)(_ max _)
  def lerp(that: Pos3d, t: Double): Pos3d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Pos3d, t: Double): Pos3d = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: Pos3d, b: Pos3d)(p: (Double, Double, Double) => Double): Pos3d =
    Pos3d(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z))
  def clamp(lower: Pos3d, upper: Pos3d): Pos3d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Pos3d$toPretty"

  def toPretty: String = "(%.2f, %.2f, %.2f)".format(x, y, z)

  def toList: List[Double] = List(x, y, z)
  def toArray: Array[Double] = Array(x, y, z)

  def toVecNd: VecNd = VecNd(toArray)
  def toVecSd: VecSd = VecSd(toArray)

  def toVec4d: Vec4d = Vec4d(x, y, z, 0.0)
  def toVec3d: Vec3d = Vec3d(x, y, z)
  def toVec2d: Vec2d = Vec2d(x, y)
  def toVec1d: Vec1d = Vec1d(x)

  def toPos3d: Pos3d = this
  def toPos2d: Pos2d = Pos2d(x, y)
  def toPos1d: Pos1d = Pos1d(x)

  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, z.toInt)
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)
  def toIndex1i: Index1i = Index1i(x.toInt)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar); buf.put(z.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x.toInt); buf.put(y.toInt); buf.put(z.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(x.toLong); buf.put(y.toLong); buf.put(z.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat); buf.put(z.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(x); buf.put(y); buf.put(z)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Pos3d {
  def apply(s: Double): Pos3d = new Pos3d(s, s, s)
  def apply(x: Double, y: Double, z: Double): Pos3d = new Pos3d(x, y, z)
  def apply(p: Pos1d, y: Double, z: Double): Pos3d = new Pos3d(p.x, y, z)
  def apply(p: Pos2d, z: Double): Pos3d = new Pos3d(p.x, p.y, z)

  def apply(ls: List[Double]): Pos3d =
    ls match {
      case List(x, y, z) => Pos3d(x, y, z)
      case List(x, y)    => Pos3d(x, y, 0.0)
      case List(x)       => Pos3d(x, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 3 components!")
    }

  def apply(ary: Array[Double]): Pos3d =
    ary match {
      case Array(x, y, z) => Pos3d(x, y, z)
      case Array(x, y)    => Pos3d(x, y, 0.0)
      case Array(x)       => Pos3d(x, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 3 components!")
    }

  def apply(buf: CharBuffer): Pos3d = Pos3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: ShortBuffer): Pos3d = Pos3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: IntBuffer): Pos3d = Pos3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: LongBuffer): Pos3d = Pos3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: FloatBuffer): Pos3d = Pos3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: DoubleBuffer): Pos3d = Pos3d(buf.get, buf.get, buf.get)

  def unapply(p: Pos3d): Some[(Double, Double, Double)] = Some((p.x, p.y, p.z))

  val origin = Pos3d(0.0)
  val size = origin.size
}
