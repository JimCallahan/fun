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

class Vec1d private (val x: Double)
    extends Tuple1[Double, Vec1d]
    with DoubleTupleOps[Vec1d]
    with VecOps[Double, Vec1d]
    with Vec {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Vec1d =>
        (that canEqual this) &&
        Scalar.equiv(x, that.x)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Vec1d => true
      case _           => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 41 + x.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Vec1d =
    i match {
      case 0 => Vec1d(e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Vec1d = Vec1d(e)

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec1d = {
    val m = mag
    if (Scalar.equiv(m, 0.0)) Vec1d(0.0)
    else (this / m)
  }

  def dot(that: Vec1d): Double = x * that.x

  def unary_- : Vec1d = Vec1d(-x)

  def +(that: Vec1d): Vec1d = Vec1d(x + that.x)
  def -(that: Vec1d): Vec1d = Vec1d(x - that.x)
  def *(that: Vec1d): Vec1d = Vec1d(x * that.x)
  def /(that: Vec1d): Vec1d = Vec1d(x / that.x)

  def +(s: Double): Vec1d = Vec1d(x + s)
  def -(s: Double): Vec1d = Vec1d(x - s)
  def *(s: Double): Vec1d = Vec1d(x * s)
  def /(s: Double): Vec1d = Vec1d(x / s)

  def forall(p: (Double) => Boolean): Boolean = p(x)
  def forall(that: Vec1d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x)
  def equiv(that: Vec1d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Vec1d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = x.isNaN

  def forany(p: (Double) => Boolean): Boolean = p(x)
  def forany(that: Vec1d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x)

  def foreach(p: (Double) => Unit): Unit = p(x)

  def map(p: (Double) => Double): Vec1d = Vec1d(p(x))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(start, x)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, start)
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = throw new UnsupportedOperationException
  def min: Double = x
  def max: Double = x

  def compwise(that: Vec1d)(p: (Double, Double) => Double): Vec1d = Vec1d(p(x, that.x))
  def min(that: Vec1d): Vec1d = compwise(that)(_ min _)
  def max(that: Vec1d): Vec1d = compwise(that)(_ max _)
  def lerp(that: Vec1d, t: Double): Vec1d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Vec1d, t: Double): Vec1d = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: Vec1d, b: Vec1d)(p: (Double, Double, Double) => Double): Vec1d = Vec1d(p(x, a.x, b.x))
  def clamp(lower: Vec1d, upper: Vec1d): Vec1d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Vec1d($toPretty)"

  def toPretty: String = "%.2f".format(x)

  def toList: List[Double] = List(x)
  def toArray: Array[Double] = Array(x)

  def toVecNd: VecNd = VecNd(toArray)
  def toVecSd: VecSd = VecSd(toArray)

  def toVec4d: Vec4d = Vec4d(x, 0.0, 0.0, 0.0)
  def toVec3d: Vec3d = Vec3d(x, 0.0, 0.0)
  def toVec2d: Vec2d = Vec2d(x, 0.0)
  def toVec1d: Vec1d = this

  def toPos3d: Pos3d = Pos3d(x, 0.0, 0.0)
  def toPos2d: Pos2d = Pos2d(x, 0.0)
  def toPos1d: Pos1d = Pos1d(x)

  def toIndex3i: Index3i = Index3i(x.toInt, 0, 0)
  def toIndex2i: Index2i = Index2i(x.toInt, 0)
  def toIndex1i: Index1i = Index1i(x.toInt)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(x.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(x)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Vec1d {
  def apply(s: Double): Vec1d = new Vec1d(s)

  def apply(ls: List[Double]): Vec1d =
    ls match {
      case List(x) => Vec1d(x)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires 1 component!")
    }

  def apply(ary: Array[Double]): Vec1d =
    ary match {
      case Array(x) => Vec1d(x)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 component!")
    }

  def apply(buf: CharBuffer): Vec1d = Vec1d(buf.get.toDouble)
  def apply(buf: ShortBuffer): Vec1d = Vec1d(buf.get.toDouble)
  def apply(buf: IntBuffer): Vec1d = Vec1d(buf.get.toDouble)
  def apply(buf: LongBuffer): Vec1d = Vec1d(buf.get.toDouble)
  def apply(buf: FloatBuffer): Vec1d = Vec1d(buf.get.toDouble)
  def apply(buf: DoubleBuffer): Vec1d = Vec1d(buf.get)

  def unapply(v: Vec1d): Some[Double] = Some(v.x)

  def random(rand: Random = new Random): Vec1d = new Vec1d(rand.nextDouble)

  @tailrec
  def randomUnit(rand: Random = new Random): Vec1d = {
    val v = random(rand) - Vec1d(0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit(rand)
  }

  val unitX = Vec1d(1.0)
  val zero = Vec1d(0.0)
  val size = zero.size
}
