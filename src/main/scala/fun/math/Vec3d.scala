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

import scala.annotation.tailrec
import scala.util.Random

class Vec3d private (val x: Double, val y: Double, val z: Double)
    extends Tuple3[Double, Vec3d]
    with Swizzle3[Vec2d, Vec3d]
    with DoubleTupleOps[Vec3d]
    with VecOps[Double, Vec3d]
    with Vec {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Vec3d =>
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
      case that: Vec3d => true
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

  def update(i: Int, e: Double): Vec3d =
    i match {
      case 0 => Vec3d(e, y, z)
      case 1 => Vec3d(x, e, z)
      case 2 => Vec3d(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Vec3d = Vec3d(e, y, z)
  def updateY(e: Double): Vec3d = Vec3d(x, e, z)
  def updateZ(e: Double): Vec3d = Vec3d(x, y, e)

  def xxx: Vec3d = Vec3d(x, x, x)
  def xxy: Vec3d = Vec3d(x, x, y)
  def xyx: Vec3d = Vec3d(x, y, x)
  def yxx: Vec3d = Vec3d(y, x, x)
  def xxz: Vec3d = Vec3d(x, x, z)
  def xzx: Vec3d = Vec3d(x, z, x)
  def zxx: Vec3d = Vec3d(z, x, x)
  def xyy: Vec3d = Vec3d(x, y, y)
  def yxy: Vec3d = Vec3d(y, x, y)
  def yyx: Vec3d = Vec3d(y, y, x)
  def xyz: Vec3d = Vec3d(x, y, z)
  def xzy: Vec3d = Vec3d(x, z, y)
  def yxz: Vec3d = Vec3d(y, x, z)
  def yzx: Vec3d = Vec3d(y, z, x)
  def zxy: Vec3d = Vec3d(z, x, y)
  def zyx: Vec3d = Vec3d(z, y, x)
  def xzz: Vec3d = Vec3d(x, z, z)
  def zxz: Vec3d = Vec3d(z, x, z)
  def zzx: Vec3d = Vec3d(z, z, x)
  def yyy: Vec3d = Vec3d(y, y, y)
  def yyz: Vec3d = Vec3d(y, y, z)
  def yzy: Vec3d = Vec3d(y, z, y)
  def zyy: Vec3d = Vec3d(z, y, y)
  def yzz: Vec3d = Vec3d(y, z, z)
  def zyz: Vec3d = Vec3d(z, y, z)
  def zzy: Vec3d = Vec3d(z, z, y)
  def zzz: Vec3d = Vec3d(z, z, z)

  def xx: Vec2d = Vec2d(x, x)
  def xy: Vec2d = Vec2d(x, y)
  def yx: Vec2d = Vec2d(y, x)
  def xz: Vec2d = Vec2d(x, z)
  def zx: Vec2d = Vec2d(z, x)
  def yy: Vec2d = Vec2d(y, y)
  def yz: Vec2d = Vec2d(y, z)
  def zy: Vec2d = Vec2d(z, y)
  def zz: Vec2d = Vec2d(z, z)

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec3d = {
    val m = mag
    if (Scalar.equiv(m, 0.0)) Vec3d(0.0)
    else (this / m)
  }

  def dot(that: Vec3d): Double = x * that.x + y * that.y + z * that.z
  def cross(that: Vec3d): Vec3d =
    Vec3d(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)

  def unary_- : Vec3d = Vec3d(-x, -y, -z)

  def +(that: Vec3d): Vec3d = Vec3d(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3d): Vec3d = Vec3d(x - that.x, y - that.y, z - that.z)
  def *(that: Vec3d): Vec3d = Vec3d(x * that.x, y * that.y, z * that.z)
  def /(that: Vec3d): Vec3d = Vec3d(x / that.x, y / that.y, z / that.z)

  def +(s: Double): Vec3d = Vec3d(x + s, y + s, z + s)
  def -(s: Double): Vec3d = Vec3d(x - s, y - s, z - s)
  def *(s: Double): Vec3d = Vec3d(x * s, y * s, z * s)
  def /(s: Double): Vec3d = Vec3d(x / s, y / s, z / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y) && p(z)
  def forall(that: Vec3d)(p: (Double, Double) => Boolean): Boolean =
    p(x, that.x) && p(y, that.y) && p(z, that.z)
  def equiv(that: Vec3d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Vec3d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = x.isNaN || y.isNaN || z.isNaN

  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y) || p(z)
  def forany(that: Vec3d)(p: (Double, Double) => Boolean): Boolean =
    p(x, that.x) || p(y, that.y) || p(z, that.z)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y); p(z) }

  def map(p: (Double) => Double): Vec3d = Vec3d(p(x), p(y), p(z))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(f(start, x), y), z)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, f(z, start)))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, p(y, z))
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Vec3d)(p: (Double, Double) => Double): Vec3d =
    Vec3d(p(x, that.x), p(y, that.y), p(z, that.z))
  def min(that: Vec3d): Vec3d = compwise(that)(_ min _)
  def max(that: Vec3d): Vec3d = compwise(that)(_ max _)
  def lerp(that: Vec3d, t: Double): Vec3d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Vec3d, t: Double): Vec3d = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: Vec3d, b: Vec3d)(p: (Double, Double, Double) => Double): Vec3d =
    Vec3d(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z))
  def clamp(lower: Vec3d, upper: Vec3d): Vec3d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Vec3d$toPretty"

  def toPretty: String = "(%.2f, %.2f, %.2f)".format(x, y, z)

  def toList: List[Double] = List(x, y, z)
  def toArray: Array[Double] = Array(x, y, z)

  def toVecNd: VecNd = VecNd(toArray)
  def toVecSd: VecSd = VecSd(toArray)

  def toVec4d: Vec4d = Vec4d(x, y, z, 0.0)
  def toVec3d: Vec3d = this
  def toVec2d: Vec2d = Vec2d(x, y)
  def toVec1d: Vec1d = Vec1d(x)

  def toPos3d: Pos3d = Pos3d(x, y, z)
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

object Vec3d {
  def apply(s: Double): Vec3d = new Vec3d(s, s, s)
  def apply(x: Double, y: Double, z: Double): Vec3d = new Vec3d(x, y, z)
  def apply(p: Vec1d, y: Double, z: Double): Vec3d = new Vec3d(p.x, y, z)
  def apply(p: Vec2d, z: Double): Vec3d = new Vec3d(p.x, p.y, z)

  def apply(ls: List[Double]): Vec3d =
    ls match {
      case List(x, y, z) => Vec3d(x, y, z)
      case List(x, y)    => Vec3d(x, y, 0.0)
      case List(x)       => Vec3d(x, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 3 components!")
    }

  def apply(ary: Array[Double]): Vec3d =
    ary match {
      case Array(x, y, z) => Vec3d(x, y, z)
      case Array(x, y)    => Vec3d(x, y, 0.0)
      case Array(x)       => Vec3d(x, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 3 components!")
    }

  def apply(buf: CharBuffer): Vec3d = Vec3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: ShortBuffer): Vec3d = Vec3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: IntBuffer): Vec3d = Vec3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: LongBuffer): Vec3d = Vec3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: FloatBuffer): Vec3d = Vec3d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: DoubleBuffer): Vec3d = Vec3d(buf.get, buf.get, buf.get)

  def unapply(v: Vec3d): Some[(Double, Double, Double)] = Some((v.x, v.y, v.z))

  def random(rand: Random = new Random): Vec3d =
    new Vec3d(rand.nextDouble, rand.nextDouble, rand.nextDouble)

  @tailrec
  def randomUnit(rand: Random = new Random): Vec3d = {
    val v = random(rand) - Vec3d(0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit(rand)
  }

  val unitX = Vec3d(1.0, 0.0, 0.0)
  val unitY = Vec3d(0.0, 1.0, 0.0)
  val unitZ = Vec3d(0.0, 0.0, 1.0)
  val zero = Vec3d(0.0)
  val size = zero.size
}
