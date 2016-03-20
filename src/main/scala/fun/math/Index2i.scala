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

class Index2i private (val x: Int, val y: Int)
    extends Tuple2[Int, Index2i]
    with TupleOps[Int, Index2i, Index2i] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Index2i =>
        (that canEqual this) && (x == that.x) && (y == that.y)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Index2i => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 43 * (41 + x.##) + y.##

  def apply(i: Int): Int =
    i match {
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Int): Index2i =
    i match {
      case 0 => Index2i(e, y)
      case 1 => Index2i(x, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Int): Index2i = Index2i(e, y)
  def updateY(e: Int): Index2i = Index2i(x, e)

  def xx: Index2i = Index2i(x, x)
  def yy: Index2i = Index2i(y, y)
  def yx: Index2i = Index2i(y, x)

  def dot(that: Index2i): Int = x * that.x + y * that.y

  def unary_- : Index2i = Index2i(-x, -y)

  def +(that: Index2i): Index2i = Index2i(x + that.x, y + that.y)
  def -(that: Index2i): Index2i = Index2i(x - that.x, y - that.y)
  def *(that: Index2i): Index2i = Index2i(x * that.x, y * that.y)
  def /(that: Index2i): Index2i = Index2i(x / that.x, y / that.y)

  def +(s: Int): Index2i = Index2i(x + s, y + s)
  def -(s: Int): Index2i = Index2i(x - s, y - s)
  def *(s: Int): Index2i = Index2i(x * s, y * s)
  def /(s: Int): Index2i = Index2i(x / s, y / s)

  def forall(p: (Int) => Boolean): Boolean = p(x) && p(y)
  def forall(that: Index2i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x) && p(y, that.y)
  def equiv(that: Index2i, epsilon: Int): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Index2i): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = false

  def forany(p: (Int) => Boolean): Boolean = p(x) || p(y)
  def forany(that: Index2i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x) || p(y, that.y)

  def foreach(p: (Int) => Unit): Unit = { p(x); p(y) }

  def map(p: (Int) => Int): Index2i = Index2i(p(x), p(y))

  def foldLeft[A](start: A)(f: (A, Int) => A): A = f(f(start, x), y)
  def /:[A](start: A)(f: (A, Int) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Int, A) => A): A = f(x, f(y, start))
  def :\[A](start: A)(f: (Int, A) => A): A = foldRight(start)(f)

  def reduce(p: (Int, Int) => Int): Int = p(x, y)
  def min: Int = reduce(_ min _)
  def max: Int = reduce(_ max _)

  def compwise(that: Index2i)(p: (Int, Int) => Int): Index2i =
    Index2i(p(x, that.x), p(y, that.y))
  def min(that: Index2i): Index2i = compwise(that)(_ min _)
  def max(that: Index2i): Index2i = compwise(that)(_ max _)

  def compwise(a: Index2i, b: Index2i)(p: (Int, Int, Int) => Int): Index2i =
    Index2i(p(x, a.x, b.x), p(y, a.y, b.y))
  def clamp(lower: Index2i, upper: Index2i): Index2i = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Index2i$toPretty"

  def toPretty: String = s"($x, $y)"

  def toList: List[Int] = List(x, y)
  def toArray: Array[Int] = Array(x, y)

  def toVecNd: VecNd = VecNd(toArray.map(_.toDouble))
  def toVecSd: VecSd = VecSd(toArray.map(_.toDouble))

  def toVec4d: Vec4d = Vec4d(x.toDouble, y.toDouble, 0.0, 0.0)
  def toVec3d: Vec3d = Vec3d(x.toDouble, y.toDouble, 0.0)
  def toVec2d: Vec2d = Vec2d(x.toDouble, y.toDouble)
  def toVec1d: Vec1d = Vec1d(x.toDouble)

  def toPos3d: Pos3d = Pos3d(x.toDouble, y.toDouble, 0.0)
  def toPos2d: Pos2d = Pos2d(x.toDouble, y.toDouble)
  def toPos1d: Pos1d = Pos1d(x.toDouble)

  def toIndex3i: Index3i = Index3i(x, y, 0)
  def toIndex2i: Index2i = this
  def toIndex1i: Index1i = Index1i(x)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x); buf.put(y)
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
    buf.put(x.toDouble); buf.put(y.toDouble)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }

}

object Index2i {
  def apply(s: Int): Index2i = new Index2i(s, s)
  def apply(x: Int, y: Int): Index2i = new Index2i(x, y)

  def apply(ls: List[Int]): Index2i =
    ls match {
      case List(x, y) => Index2i(x, y)
      case List(x)    => Index2i(x, 0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 2 components!")
    }

  def apply(ary: Array[Int]): Index2i =
    ary match {
      case Array(x, y) => Index2i(x, y)
      case Array(x)    => Index2i(x, 0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 2 components!")
    }

  def apply(buf: CharBuffer): Index2i = Index2i(buf.get.toInt, buf.get.toInt)
  def apply(buf: ShortBuffer): Index2i = Index2i(buf.get.toInt, buf.get.toInt)
  def apply(buf: IntBuffer): Index2i = Index2i(buf.get, buf.get)
  def apply(buf: LongBuffer): Index2i = Index2i(buf.get.toInt, buf.get.toInt)
  def apply(buf: FloatBuffer): Index2i = Index2i(buf.get.toInt, buf.get.toInt)
  def apply(buf: DoubleBuffer): Index2i = Index2i(buf.get.toInt, buf.get.toInt)

  def unapply(a: Index2i): Some[(Int, Int)] = Some((a.x, a.y))

  val zero = Index2i(0)
  val size = zero.size
}
