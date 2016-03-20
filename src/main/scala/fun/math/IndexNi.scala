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

import java.nio.CharBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.ShortBuffer

/** A vector of arbitrary size containing Int data. */
class IndexNi private (private val vs: Array[Int])
    extends TupleAccess[Int, IndexNi]
    with TupleOps[Int, IndexNi, IndexNi] {

  def size: Int = vs.size

  override def equals(that: Any): Boolean =
    that match {
      case that: IndexNi =>
        (that canEqual this) && arrayEquals(vs, that.vs)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: IndexNi => true
      case _             => false
    }

  override def hashCode: Int = vs.hashCode

  def apply(i: Int): Int = vs(i)

  def update(i: Int, e: Int): IndexNi = {
    val v = vs.clone
    v(i) = e
    IndexNi(v)
  }

  def dot(that: IndexNi): Int =
    validate(that) {
      @tailrec def f(i: Int, tl: Int): Int = if (i >= size) tl else f(i + 1, tl + this(i) * that(i))
      f(0, 0)
    }

  def unary_- : IndexNi = IndexNi(vs.map(-_))

  def +(that: IndexNi): IndexNi = compwise(that)(_ + _)
  def -(that: IndexNi): IndexNi = compwise(that)(_ + _)
  def *(that: IndexNi): IndexNi = compwise(that)(_ + _)
  def /(that: IndexNi): IndexNi = compwise(that)(_ + _)

  def +(s: Int): IndexNi = IndexNi(vs.map(_ + s))
  def -(s: Int): IndexNi = IndexNi(vs.map(_ - s))
  def *(s: Int): IndexNi = IndexNi(vs.map(_ * s))
  def /(s: Int): IndexNi = IndexNi(vs.map(_ / s))

  def forall(p: (Int) => Boolean): Boolean = vs.forall(p)

  def forall(that: IndexNi)(p: (Int, Int) => Boolean): Boolean = {
    @tailrec def f(i: Int): Boolean = if (i >= size) true else p(this(i), that(i)) && f(i + 1)
    f(0)
  }

  def equiv(that: IndexNi, epsilon: Int): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: IndexNi): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = false

  def forany(p: (Int) => Boolean): Boolean = forall(!p(_))
  def forany(that: IndexNi)(p: (Int, Int) => Boolean): Boolean = forall(that)(!p(_, _))

  def foreach(p: (Int) => Unit): Unit = vs.foreach(p)

  def map(p: (Int) => Int): IndexNi = IndexNi(vs.map(p))

  def foldLeft[A](start: A)(f: (A, Int) => A): A = vs.foldLeft(start)(f)
  def /:[A](start: A)(f: (A, Int) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Int, A) => A): A = vs.foldRight(start)(f)
  def :\[A](start: A)(f: (Int, A) => A): A = foldRight(start)(f)

  def reduce(p: (Int, Int) => Int): Int = vs.reduce(p)
  def min: Int = reduce(_ min _)
  def max: Int = reduce(_ max _)

  def compwise(that: IndexNi)(p: (Int, Int) => Int): IndexNi =
    validate(that) {
      IndexNi(Array.tabulate(size)(i => p(this(i), that(i))))
    }

  def min(that: IndexNi): IndexNi = compwise(that)(_ min _)
  def max(that: IndexNi): IndexNi = compwise(that)(_ max _)

  def compwise(a: IndexNi, b: IndexNi)(p: (Int, Int, Int) => Int): IndexNi =
    validate(a) {
      validate(b) {
        IndexNi(Array.tabulate(size)(i => p(this(i), a(i), b(i))))
      }
    }

  def clamp(lower: IndexNi, upper: IndexNi): IndexNi = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"IndexNi$toPretty"

  override def toPretty = "(" + (vs.map("%d".format(_)).mkString(", ")) + ")"

  def toList: List[Int] = vs.toList
  def toArray: Array[Int] = vs.clone

  def getOrZero(i: Int): Int = if (i < size) this(i) else 0

  def toVecNd: VecNd = VecNd(vs.map(_.toDouble))
  def toVecSd: VecSd = VecSd(vs.map(_.toDouble))

  def toVec4d: Vec4d = Vec4d(getOrZero(0), getOrZero(1), getOrZero(2), getOrZero(3))
  def toVec3d: Vec3d = Vec3d(getOrZero(0), getOrZero(1), getOrZero(2))
  def toVec2d: Vec2d = Vec2d(getOrZero(0), getOrZero(1))
  def toVec1d: Vec1d = Vec1d(getOrZero(0))

  def toPos3d: Pos3d = toVec3d.toPos3d
  def toPos2d: Pos2d = toVec2d.toPos2d
  def toPos1d: Pos1d = toVec1d.toPos1d

  def toIndexNi: IndexNi = this
  // def toIndexSi = IndexSi(vs)
  def toIndexNl: IndexNl = IndexNl(vs.map(_.toLong))
  // def toIndexSl =IndexNl(vs.map(_.toLong))

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
    foreach(e => buf.put(e))
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
    foreach(e => buf.put(e.toDouble))
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object IndexNi {
  def apply(ary: Array[Int]): IndexNi = new IndexNi(ary)
  def apply(v: Int*): IndexNi = new IndexNi(v.toArray)
  def apply(size: Int, s: Int): IndexNi = new IndexNi(Array.fill(size)(s))

  def apply(size: Int, buf: CharBuffer): IndexNi = IndexNi(Array.tabulate(size)(_ => buf.get.toInt))
  def apply(size: Int, buf: ShortBuffer): IndexNi = IndexNi(Array.tabulate(size)(_ => buf.get.toInt))
  def apply(size: Int, buf: IntBuffer): IndexNi = IndexNi(Array.tabulate(size)(_ => buf.get))
  def apply(size: Int, buf: LongBuffer): IndexNi = IndexNi(Array.tabulate(size)(_ => buf.get.toInt))
  def apply(size: Int, buf: FloatBuffer): IndexNi = IndexNi(Array.tabulate(size)(_ => buf.get.toInt))
  def apply(size: Int, buf: DoubleBuffer): IndexNi = IndexNi(Array.tabulate(size)(_ => buf.get.toInt))

  def unapplySeq(a: IndexNi): Some[Seq[Int]] = Some(a.vs.toSeq)

  def zero(size: Int): IndexNi = IndexNi(size, 0)
}
