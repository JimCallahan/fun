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

class Pos1d private (val x: Double)
    extends Tuple1[Double, Pos1d]
    with DoubleTupleOps[Pos1d]
    with PosOps[Double, Pos1d, Vec1d]
    with Pos {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Pos1d =>
        (that canEqual this) &&
        Scalar.equiv(x, that.x)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Pos1d => true
      case _           => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 41 + x.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Pos1d =
    i match {
      case 0 => Pos1d(e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Pos1d = Pos1d(e)

  def unary_- : Pos1d = Pos1d(-x)

  def -(that: Pos1d): Vec1d = Vec1d(x - that.x)

  def +(that: Vec1d): Pos1d = Pos1d(x + that.x)
  def -(that: Vec1d): Pos1d = Pos1d(x - that.x)
  def *(that: Vec1d): Pos1d = Pos1d(x * that.x)
  def /(that: Vec1d): Pos1d = Pos1d(x / that.x)

  def +(s: Double): Pos1d = Pos1d(x + s)
  def -(s: Double): Pos1d = Pos1d(x - s)
  def *(s: Double): Pos1d = Pos1d(x * s)
  def /(s: Double): Pos1d = Pos1d(x / s)

  def forall(p: (Double) => Boolean): Boolean = p(x)
  def forall(that: Pos1d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x)
  def equiv(that: Pos1d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Pos1d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = x.isNaN

  def forany(p: (Double) => Boolean): Boolean = p(x)
  def forany(that: Pos1d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x)

  def foreach(p: (Double) => Unit): Unit = p(x)

  def map(p: (Double) => Double): Pos1d = Pos1d(p(x))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(start, x)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, start)
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = throw new UnsupportedOperationException
  def min: Double = x
  def max: Double = x

  def compwise(that: Pos1d)(p: (Double, Double) => Double): Pos1d = Pos1d(p(x, that.x))
  def min(that: Pos1d): Pos1d = compwise(that)(_ min _)
  def max(that: Pos1d): Pos1d = compwise(that)(_ max _)
  def lerp(that: Pos1d, t: Double): Pos1d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Pos1d, t: Double): Pos1d = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: Pos1d, b: Pos1d)(p: (Double, Double, Double) => Double): Pos1d = Pos1d(p(x, a.x, b.x))
  def clamp(lower: Pos1d, upper: Pos1d): Pos1d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Pos1d($toPretty)"

  def toPretty: String = "%.2f".format(x)

  def toList: List[Double] = List(x)
  def toArray: Array[Double] = Array(x)

  def toVecNd: VecNd = VecNd(toArray)
  def toVecSd: VecSd = VecSd(toArray)

  def toVec4d: Vec4d = Vec4d(x, 0.0, 0.0, 0.0)
  def toVec3d: Vec3d = Vec3d(x, 0.0, 0.0)
  def toVec2d: Vec2d = Vec2d(x, 0.0)
  def toVec1d: Vec1d = Vec1d(x)

  def toPos3d: Pos3d = Pos3d(x, 0.0, 0.0)
  def toPos2d: Pos2d = Pos2d(x, 0.0)
  def toPos1d: Pos1d = this

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

object Pos1d {
  def apply(x: Double): Pos1d = new Pos1d(x)

  def apply(ls: List[Double]): Pos1d =
    ls match {
      case List(x) => Pos1d(x)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires 1 component!")
    }

  def apply(ary: Array[Double]): Pos1d =
    ary match {
      case Array(x) => Pos1d(x)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 component!")
    }

  def apply(buf: CharBuffer): Pos1d = Pos1d(buf.get.toDouble)
  def apply(buf: ShortBuffer): Pos1d = Pos1d(buf.get.toDouble)
  def apply(buf: IntBuffer): Pos1d = Pos1d(buf.get.toDouble)
  def apply(buf: LongBuffer): Pos1d = Pos1d(buf.get.toDouble)
  def apply(buf: FloatBuffer): Pos1d = Pos1d(buf.get.toDouble)
  def apply(buf: DoubleBuffer): Pos1d = Pos1d(buf.get)

  def unapply(p: Pos1d): Some[Double] = Some(p.x)

  val origin = Pos1d(0.0)
  val size = origin.size
}
