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

class Index1i private (val x: Int)
    extends Tuple1[Int, Index1i]
    with TupleOps[Int, Index1i, Index1i] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Index1i =>
        (that canEqual this) && (x == that.x)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Index1i => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 41 + x.##

  def apply(i: Int): Int =
    i match {
      case 0 => x
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Int): Index1i =
    i match {
      case 0 => Index1i(e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Int): Index1i = Index1i(e)

  def dot(that: Index1i): Int = x * that.x

  def unary_- : Index1i = Index1i(-x)

  def +(that: Index1i): Index1i = Index1i(x + that.x)
  def -(that: Index1i): Index1i = Index1i(x - that.x)
  def *(that: Index1i): Index1i = Index1i(x * that.x)
  def /(that: Index1i): Index1i = Index1i(x / that.x)

  def +(s: Int): Index1i = Index1i(x + s)
  def -(s: Int): Index1i = Index1i(x - s)
  def *(s: Int): Index1i = Index1i(x * s)
  def /(s: Int): Index1i = Index1i(x / s)

  def forall(p: (Int) => Boolean): Boolean = p(x)
  def forall(that: Index1i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x)
  def equiv(that: Index1i, epsilon: Int): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Index1i): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = false

  def forany(p: (Int) => Boolean): Boolean = p(x)
  def forany(that: Index1i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x)

  def foreach(p: (Int) => Unit): Unit = p(x)

  def map(p: (Int) => Int): Index1i = Index1i(p(x))

  def foldLeft[A](start: A)(f: (A, Int) => A): A = f(start, x)
  def /:[A](start: A)(f: (A, Int) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Int, A) => A): A = f(x, start)
  def :\[A](start: A)(f: (Int, A) => A): A = foldRight(start)(f)

  def reduce(p: (Int, Int) => Int): Int = throw new UnsupportedOperationException
  def min: Int = x
  def max: Int = x

  def compwise(that: Index1i)(p: (Int, Int) => Int): Index1i = Index1i(p(x, that.x))
  def min(that: Index1i): Index1i = compwise(that)(_ min _)
  def max(that: Index1i): Index1i = compwise(that)(_ max _)

  def compwise(a: Index1i, b: Index1i)(p: (Int, Int, Int) => Int): Index1i = Index1i(p(x, a.x, b.x))
  def clamp(lower: Index1i, upper: Index1i): Index1i = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Index1i($toPretty)"

  def toPretty: String = x.toString

  def toList: List[Int] = List(x)
  def toArray: Array[Int] = Array(x)

  def toVecNd: VecNd = VecNd(toArray.map(_.toDouble))
  def toVecSd: VecSd = VecSd(toArray.map(_.toDouble))

  def toVec4d: Vec4d = Vec4d(x.toDouble, 0.0, 0.0, 0.0)
  def toVec3d: Vec3d = Vec3d(x.toDouble, 0.0, 0.0)
  def toVec2d: Vec2d = Vec2d(x.toDouble, 0.0)
  def toVec1d: Vec1d = Vec1d(x.toDouble)

  def toPos3d: Pos3d = Pos3d(x.toDouble, 0.0, 0.0)
  def toPos2d: Pos2d = Pos2d(x.toDouble, 0.0)
  def toPos1d: Pos1d = Pos1d(x.toDouble)

  def toIndex3i: Index3i = Index3i(x, 0, 0)
  def toIndex2i: Index2i = Index2i(x, 0)
  def toIndex1i: Index1i = this

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x)
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
    buf.put(x.toDouble)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }

}

object Index1i {
  def apply(s: Int): Index1i = new Index1i(s)

  def apply(ls: List[Int]): Index1i =
    ls match {
      case List(x) => Index1i(x)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires 1 component!")
    }

  def apply(ary: Array[Int]): Index1i =
    ary match {
      case Array(x) => Index1i(x)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires 1 component!")
    }

  def apply(buf: CharBuffer): Index1i = Index1i(buf.get.toInt)
  def apply(buf: ShortBuffer): Index1i = Index1i(buf.get.toInt)
  def apply(buf: IntBuffer): Index1i = Index1i(buf.get)
  def apply(buf: LongBuffer): Index1i = Index1i(buf.get.toInt)
  def apply(buf: FloatBuffer): Index1i = Index1i(buf.get.toInt)
  def apply(buf: DoubleBuffer): Index1i = Index1i(buf.get.toInt)

  def unapply(a: Index1i): Some[Int] = Some(a.x)

  val zero = Index1i(0)
  val size = zero.size
}
