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

class Vec2d private (val x: Double, val y: Double)
    extends Tuple2[Double, Vec2d]
    with Swizzle2[Vec2d]
    with DoubleTupleOps[Vec2d]
    with VecOps[Double, Vec2d]
    with Vec {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Vec2d =>
        (that canEqual this) &&
        Scalar.equiv(x, that.x) &&
        Scalar.equiv(y, that.y)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Vec2d => true
      case _           => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 43 * (41 + x.##) + y.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Vec2d =
    i match {
      case 0 => Vec2d(e, y)
      case 1 => Vec2d(x, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Vec2d = Vec2d(e, y)
  def updateY(e: Double): Vec2d = Vec2d(x, e)

  def xx: Vec2d = Vec2d(x, x)
  def yy: Vec2d = Vec2d(y, y)
  def yx: Vec2d = Vec2d(y, x)

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec2d = {
    val m = mag
    if (Scalar.equiv(m, 0.0)) Vec2d(0.0)
    else (this / m)
  }

  def dot(that: Vec2d): Double = x * that.x + y * that.y

  def unary_- : Vec2d = Vec2d(-x, -y)

  def +(that: Vec2d): Vec2d = Vec2d(x + that.x, y + that.y)
  def -(that: Vec2d): Vec2d = Vec2d(x - that.x, y - that.y)
  def *(that: Vec2d): Vec2d = Vec2d(x * that.x, y * that.y)
  def /(that: Vec2d): Vec2d = Vec2d(x / that.x, y / that.y)

  def +(s: Double): Vec2d = Vec2d(x + s, y + s)
  def -(s: Double): Vec2d = Vec2d(x - s, y - s)
  def *(s: Double): Vec2d = Vec2d(x * s, y * s)
  def /(s: Double): Vec2d = Vec2d(x / s, y / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y)
  def forall(that: Vec2d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) && p(y, that.y)
  def equiv(that: Vec2d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Vec2d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = x.isNaN || y.isNaN

  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y)
  def forany(that: Vec2d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) || p(y, that.y)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y) }

  def map(p: (Double) => Double): Vec2d = Vec2d(p(x), p(y))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(start, x), y)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, start))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, y)
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Vec2d)(p: (Double, Double) => Double): Vec2d =
    Vec2d(p(x, that.x), p(y, that.y))
  def min(that: Vec2d): Vec2d = compwise(that)(_ min _)
  def max(that: Vec2d): Vec2d = compwise(that)(_ max _)
  def lerp(that: Vec2d, t: Double): Vec2d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Vec2d, t: Double): Vec2d = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: Vec2d, b: Vec2d)(p: (Double, Double, Double) => Double): Vec2d =
    Vec2d(p(x, a.x, b.x), p(y, a.y, b.y))
  def clamp(lower: Vec2d, upper: Vec2d): Vec2d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Vec2d$toPretty"

  def toPretty: String = "(%.2f, %.2f)".format(x, y)

  def toList: List[Double] = List(x, y)
  def toArray: Array[Double] = Array(x, y)

  def toVecNd: VecNd = VecNd(toArray)
  def toVecSd: VecSd = VecSd(toArray)

  def toVec4d: Vec4d = Vec4d(x, y, 0.0, 0.0)
  def toVec3d: Vec3d = Vec3d(x, y, 0.0)
  def toVec2d: Vec2d = this
  def toVec1d: Vec1d = Vec1d(x)

  def toPos3d: Pos3d = Pos3d(x, y, 0.0)
  def toPos2d: Pos2d = Pos2d(x, y)
  def toPos1d: Pos1d = Pos1d(x)

  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, 0)
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)
  def toIndex1i: Index1i = Index1i(x.toInt)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x.toInt); buf.put(y.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(x.toLong); buf.put(y.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(x); buf.put(y)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Vec2d {
  def apply(s: Double): Vec2d = new Vec2d(s, s)
  def apply(x: Double, y: Double): Vec2d = new Vec2d(x, y)
  def apply(p: Vec1d, y: Double): Vec2d = new Vec2d(p.x, y)

  def apply(ls: List[Double]): Vec2d =
    ls match {
      case List(x, y) => Vec2d(x, y)
      case List(x)    => Vec2d(x, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 2 components!")
    }

  def apply(ary: Array[Double]): Vec2d =
    ary match {
      case Array(x, y) => Vec2d(x, y)
      case Array(x)    => Vec2d(x, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 2 components!")
    }

  def apply(buf: CharBuffer): Vec2d = Vec2d(buf.get.toDouble, buf.get.toDouble)
  def apply(buf: ShortBuffer): Vec2d = Vec2d(buf.get.toDouble, buf.get.toDouble)
  def apply(buf: IntBuffer): Vec2d = Vec2d(buf.get.toDouble, buf.get.toDouble)
  def apply(buf: LongBuffer): Vec2d = Vec2d(buf.get.toDouble, buf.get.toDouble)
  def apply(buf: FloatBuffer): Vec2d = Vec2d(buf.get.toDouble, buf.get.toDouble)
  def apply(buf: DoubleBuffer): Vec2d = Vec2d(buf.get, buf.get)

  def unapply(v: Vec2d): Some[(Double, Double)] = Some((v.x, v.y))

  def random(rand: Random = new Random): Vec2d = new Vec2d(rand.nextDouble, rand.nextDouble)

  @tailrec
  def randomUnit(rand: Random = new Random): Vec2d = {
    val v = random(rand) - Vec2d(0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit(rand)
  }

  val unitX = Vec2d(1.0, 0.0)
  val unitY = Vec2d(0.0, 1.0)
  val zero = Vec2d(0.0)
  val size = zero.size

  /** The clockwise angle (in radians) between two vectors. */
  def clockwise(a: Vec2d, b: Vec2d): Double = {
    import scala.math.{ acos, Pi }
    val na = a.normalized
    val nb = b.normalized
    val dp = na dot nb
    if (Scalar.equiv(dp, 1.0)) 0.0
    else {
      val theta = acos(dp)
      if ((na.toVec3d cross nb.toVec3d).z <= 0.0) theta
      else 2.0 * Pi - theta
    }
  }
}
