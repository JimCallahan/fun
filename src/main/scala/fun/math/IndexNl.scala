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

/** A vector of arbitrary size containing Long data. */
class IndexNl private (private val vs: Array[Long])
    extends TupleAccess[Long, IndexNl]
    with TupleOps[Long, IndexNl, IndexNl] {

  def size: Int = vs.size

  override def equals(that: Any): Boolean =
    that match {
      case that: IndexNl =>
        (that canEqual this) && arrayEquals(vs, that.vs)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: IndexNl => true
      case _             => false
    }

  override def hashCode: Int = vs.hashCode

  def apply(i: Int): Long = vs(i)

  def update(i: Int, e: Long): IndexNl = {
    val v = vs.clone
    v(i) = e
    IndexNl(v)
  }

  def dot(that: IndexNl): Long =
    validate(that) {
      @tailrec def f(i: Int, tl: Long): Long = if (i >= size) tl else f(i + 1, tl + this(i) * that(i))
      f(0, 0)
    }

  def unary_- : IndexNl = IndexNl(vs.map(-_))

  def +(that: IndexNl): IndexNl = compwise(that)(_ + _)
  def -(that: IndexNl): IndexNl = compwise(that)(_ + _)
  def *(that: IndexNl): IndexNl = compwise(that)(_ + _)
  def /(that: IndexNl): IndexNl = compwise(that)(_ + _)

  def +(s: Long): IndexNl = IndexNl(vs.map(_ + s))
  def -(s: Long): IndexNl = IndexNl(vs.map(_ - s))
  def *(s: Long): IndexNl = IndexNl(vs.map(_ * s))
  def /(s: Long): IndexNl = IndexNl(vs.map(_ / s))

  def forall(p: (Long) => Boolean): Boolean = vs.forall(p)

  def forall(that: IndexNl)(p: (Long, Long) => Boolean): Boolean = {
    @tailrec def f(i: Int): Boolean = if (i >= size) true else p(this(i), that(i)) && f(i + 1)
    f(0)
  }

  def equiv(that: IndexNl, epsilon: Long): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: IndexNl): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = false

  def forany(p: (Long) => Boolean): Boolean = forall(!p(_))
  def forany(that: IndexNl)(p: (Long, Long) => Boolean): Boolean = forall(that)(!p(_, _))

  def foreach(p: (Long) => Unit): Unit = vs.foreach(p)

  def map(p: (Long) => Long): IndexNl = IndexNl(vs.map(p))

  def foldLeft[A](start: A)(f: (A, Long) => A): A = vs.foldLeft(start)(f)
  def /:[A](start: A)(f: (A, Long) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Long, A) => A): A = vs.foldRight(start)(f)
  def :\[A](start: A)(f: (Long, A) => A): A = foldRight(start)(f)

  def reduce(p: (Long, Long) => Long): Long = vs.reduce(p)
  def min: Long = reduce(_ min _)
  def max: Long = reduce(_ max _)

  def compwise(that: IndexNl)(p: (Long, Long) => Long): IndexNl =
    validate(that) {
      IndexNl(Array.tabulate(size)(i => p(this(i), that(i))))
    }

  def min(that: IndexNl): IndexNl = compwise(that)(_ min _)
  def max(that: IndexNl): IndexNl = compwise(that)(_ max _)

  def compwise(a: IndexNl, b: IndexNl)(p: (Long, Long, Long) => Long): IndexNl =
    validate(a) {
      validate(b) {
        IndexNl(Array.tabulate(size)(i => p(this(i), a(i), b(i))))
      }
    }

  def clamp(lower: IndexNl, upper: IndexNl): IndexNl = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"IndexNl$toPretty"

  def toPretty: String = "(" + (vs.map("%d".format(_)).mkString(", ")) + ")"

  def toList: List[Long] = vs.toList
  def toArray: Array[Long] = vs.clone

  def getOrZero(i: Int): Long = if (i < size) this(i) else 0

  def toVecNd: VecNd = VecNd(vs.map(_.toDouble))
  def toVecSd: VecSd = VecSd(vs.map(_.toDouble))

  def toVec4d: Vec4d = Vec4d(getOrZero(0), getOrZero(1), getOrZero(2), getOrZero(3))
  def toVec3d: Vec3d = Vec3d(getOrZero(0), getOrZero(1), getOrZero(2))
  def toVec2d: Vec2d = Vec2d(getOrZero(0), getOrZero(1))
  def toVec1d: Vec1d = Vec1d(getOrZero(0))

  def toPos3d: Pos3d = toVec3d.toPos3d
  def toPos2d: Pos2d = toVec2d.toPos2d
  def toPos1d: Pos1d = toVec1d.toPos1d

  def toIndexNi: IndexNi = IndexNi(vs.map(_.toInt))
  // def toIndexSi = IndexNi(vs.map(_.toInt))
  def toIndexNl: IndexNl = this
  // def toIndexSl = IndexSl(vs)

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
    foreach(e => buf.put(e))
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

object IndexNl {
  def apply(ary: Array[Long]): IndexNl = new IndexNl(ary)
  def apply(v: Long*): IndexNl = new IndexNl(v.toArray)
  def apply(size: Int, s: Long): IndexNl = new IndexNl(Array.fill(size)(s))

  def apply(size: Int, buf: CharBuffer): IndexNl = IndexNl(Array.tabulate(size)(_ => buf.get.toLong))
  def apply(size: Int, buf: ShortBuffer): IndexNl = IndexNl(Array.tabulate(size)(_ => buf.get.toLong))
  def apply(size: Int, buf: IntBuffer): IndexNl = IndexNl(Array.tabulate(size)(_ => buf.get.toLong))
  def apply(size: Int, buf: LongBuffer): IndexNl = IndexNl(Array.tabulate(size)(_ => buf.get))
  def apply(size: Int, buf: FloatBuffer): IndexNl = IndexNl(Array.tabulate(size)(_ => buf.get.toLong))
  def apply(size: Int, buf: DoubleBuffer): IndexNl = IndexNl(Array.tabulate(size)(_ => buf.get.toLong))

  def unapplySeq(a: IndexNl): Some[Seq[Long]] = Some(a.vs.toSeq)

  def zero(size: Int): IndexNl = IndexNl(size, 0)
}
