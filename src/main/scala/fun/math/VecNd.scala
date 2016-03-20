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

/** A vector of arbitrary size containing Double data. */
class VecNd private (private val vs: Array[Double])
    extends TupleAccess[Double, VecNd]
    with DoubleTupleOps[VecNd]
    with VecOps[Double, VecNd]
    with Vec {

  def size: Int = vs.size

  override def equals(that: Any): Boolean =
    that match {
      case that: VecNd =>
        (that canEqual this) && arrayEquals(vs, that.vs)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: VecNd => true
      case _           => false
    }

  override def hashCode: Int = vs.hashCode

  def apply(i: Int): Double = vs(i)

  def update(i: Int, e: Double): VecNd = {
    val v = vs.clone
    v(i) = e
    VecNd(v)
  }

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: VecNd = {
    val m = mag
    if (Scalar.equiv(m, 0.0)) VecNd(size, 0.0)
    else (this / m)
  }

  def dot(that: VecNd): Double =
    validate(that) {
      @tailrec def f(i: Int, tl: Double): Double = if (i >= size) tl else f(i + 1, tl + this(i) * that(i))
      f(0, 0.0)
    }

  def unary_- : VecNd = VecNd(vs.map(-_))

  def +(that: VecNd): VecNd = compwise(that)(_ + _)
  def -(that: VecNd): VecNd = compwise(that)(_ - _)
  def *(that: VecNd): VecNd = compwise(that)(_ * _)
  def /(that: VecNd): VecNd = compwise(that)(_ / _)

  def +(s: Double): VecNd = VecNd(vs.map(_ + s))
  def -(s: Double): VecNd = VecNd(vs.map(_ - s))
  def *(s: Double): VecNd = VecNd(vs.map(_ * s))
  def /(s: Double): VecNd = VecNd(vs.map(_ / s))

  def forall(p: (Double) => Boolean): Boolean = vs.forall(p)

  def forall(that: VecNd)(p: (Double, Double) => Boolean): Boolean = {
    @tailrec def f(i: Int): Boolean = if (i >= size) true else p(this(i), that(i)) && f(i + 1)
    f(0)
  }

  def equiv(that: VecNd, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: VecNd): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = forany(_.isNaN)

  def forany(p: (Double) => Boolean): Boolean = forall(!p(_))
  def forany(that: VecNd)(p: (Double, Double) => Boolean): Boolean = forall(that)(!p(_, _))

  def foreach(p: (Double) => Unit): Unit = vs.foreach(p)

  def map(p: (Double) => Double): VecNd = VecNd(vs.map(p))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = vs.foldLeft(start)(f)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = vs.foldRight(start)(f)
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = vs.reduce(p)
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: VecNd)(p: (Double, Double) => Double): VecNd =
    validate(that) {
      VecNd(Array.tabulate(size)(i => p(this(i), that(i))))
    }

  def min(that: VecNd): VecNd = compwise(that)(_ min _)
  def max(that: VecNd): VecNd = compwise(that)(_ max _)
  def lerp(that: VecNd, t: Double): VecNd = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: VecNd, t: Double): VecNd = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: VecNd, b: VecNd)(p: (Double, Double, Double) => Double): VecNd =
    validate(a) {
      validate(b) {
        VecNd(Array.tabulate(size)(i => p(this(i), a(i), b(i))))
      }
    }

  def clamp(lower: VecNd, upper: VecNd): VecNd = compwise(lower, upper)(Scalar.clamp)

  /** The components of this vector followed be the components of that vector. */
  def join(that: VecNd): VecNd = VecNd(vs ++ that.vs)

  /** Convert to a String representation */
  override def toString = s"VecNd$toPretty"

  def toPretty: String = "(" + (vs.map("%.2f".format(_)).mkString(", ")) + ")"

  def toList: List[Double] = vs.toList
  def toArray: Array[Double] = vs.clone

  def getOrZero(i: Int): Double = if (i < size) this(i) else 0.0

  def toVecNd: VecNd = this
  def toVecSd: VecSd = VecSd(vs)
  def toVecSd(keep: Double => Boolean): VecSd = VecSd(vs, keep)

  def toVec4d: Vec4d = Vec4d(getOrZero(0), getOrZero(1), getOrZero(2), getOrZero(3))
  def toVec3d: Vec3d = Vec3d(getOrZero(0), getOrZero(1), getOrZero(2))
  def toVec2d: Vec2d = Vec2d(getOrZero(0), getOrZero(1))
  def toVec1d: Vec1d = Vec1d(getOrZero(0))

  def toPos3d: Pos3d = toVec3d.toPos3d
  def toPos2d: Pos2d = toVec2d.toPos2d
  def toPos1d: Pos1d = toVec1d.toPos1d

  def toIndex3i: Index3i = toVec3d.toIndex3i
  def toIndex2i: Index2i = toVec2d.toIndex2i
  def toIndex1i: Index1i = toVec1d.toIndex1i

  def putNative(buf: CharBuffer) {
    foreach(e => buf.put(e.toChar))
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    foreach(e => buf.put(e.toShort))
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    foreach(e => buf.put(e.toInt))
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    foreach(e => buf.put(e.toLong))
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    foreach(e => buf.put(e.toFloat))
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    foreach(e => buf.put(e))
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object VecNd {
  def apply(ary: Array[Double]): VecNd = new VecNd(ary)
  def apply(seq: IndexedSeq[Double]): VecNd = new VecNd(seq.toArray)
  def apply(v: Double*): VecNd = new VecNd(v.toArray)
  def apply(size: Int, s: Double): VecNd = new VecNd(Array.fill(size)(s))

  def apply(size: Int, buf: CharBuffer): VecNd = VecNd(Array.tabulate(size)(_ => buf.get.toDouble))
  def apply(size: Int, buf: ShortBuffer): VecNd = VecNd(Array.tabulate(size)(_ => buf.get.toDouble))
  def apply(size: Int, buf: IntBuffer): VecNd = VecNd(Array.tabulate(size)(_ => buf.get.toDouble))
  def apply(size: Int, buf: LongBuffer): VecNd = VecNd(Array.tabulate(size)(_ => buf.get.toDouble))
  def apply(size: Int, buf: FloatBuffer): VecNd = VecNd(Array.tabulate(size)(_ => buf.get.toDouble))
  def apply(size: Int, buf: DoubleBuffer): VecNd = VecNd(Array.tabulate(size)(_ => buf.get))

  def unapplySeq(v: VecNd): Some[Seq[Double]] = Some(v.vs.toSeq)

  def random(size: Int, rand: Random = new Random): VecNd =
    new VecNd(Array.fill(size)(rand.nextDouble))

  @tailrec
  def randomUnit(size: Int, rand: Random = new Random): VecNd = {
    val v = random(size, rand) - VecNd(size, 0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit(size, rand)
  }

  def zero(size: Int): VecNd = VecNd(size, 0.0)
}
