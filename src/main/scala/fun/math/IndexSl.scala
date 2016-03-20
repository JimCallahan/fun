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
import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuilder

import java.nio.CharBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.ShortBuffer

/** An immutable sparse vector of arbitrary size containing Long data.
  *
  * Storage and performance of most operations is proportional to the number of specified elements
  * rather than the total number of elements as with dense vectors.  The per element cost is slightly
  * higher than for dense vectors.  For good performance, sparse vector should only be used when the
  * majority of elements are not specified.  Unspecified elements are assumed to be zero valued.
  *
  * NOTE: Unlike dense vector, the cost of apply() and update() operations are proportional to the number
  * of specified elements and not constant time.  Therefore, code which iterates over indices using these
  * operations to lookup and/or modify elements should be avoided.
  */
class IndexSl private (private val bits: BitSet, private val vs: Array[Long])
    extends TupleAccess[Long, IndexSl]
    with TupleOps[Long, IndexSl, IndexSl] {

  def size: Int = bits.size

  override def equals(that: Any): Boolean =
    that match {
      case that: IndexSl =>
        (that canEqual this) && (bits == that.bits) && arrayEquals(vs, that.vs)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: IndexSl => true
      case _             => false
    }

  override def hashCode: Int = 43 * (41 + bits.##) + vs.##

  def apply(i: Int): Long = {
    val iter = bits.iterator
    var j = -1
    var b = -1
    while ((b < i) && iter.hasNext) {
      b = iter.next
      j += 1
    }
    if (b == i) vs(j) else 0L
  }

  def update(i: Int, e: Long): IndexSl = {
    val iter = bits.iterator
    var j = -1
    var b = -1
    while ((b < i) && iter.hasNext) {
      b = iter.next
      j += 1
    }

    if (b == i) {
      val v = vs.clone
      v(j) = e
      new IndexSl(bits, v)
    }
    else {
      val nbits = bits + i
      bits.toArray.indexWhere(_ > i) match {
        case -1 => new IndexSl(nbits, vs :+ e)
        case n =>
          val abd = new ArrayBuilder.ofLong
          for (i <- 0 until n) abd += vs(i)
          abd += e
          for (i <- n until vs.size) abd += vs(i)
          new IndexSl(nbits, abd.result)
      }
    }
  }

  def dot(that: IndexSl): Long = {
    val at = bits.iterator
    val avt = vs.iterator
    val bt = that.bits.iterator
    val bvt = that.vs.iterator
    @tailrec def f(ai: Int, bi: Int, tl: Long): Long =
      if ((ai < bi) && at.hasNext && avt.hasNext) {
        avt.next
        f(at.next, bi, tl)
      }
      else if ((bi < ai) && bt.hasNext && bvt.hasNext) {
        bvt.next
        f(ai, bt.next, tl)
      }
      else if (at.hasNext && avt.hasNext && bt.hasNext && bvt.hasNext)
        f(at.next, bt.next, avt.next * bvt.next + tl)
      else tl

    f(-1, -1, 0L)
  }

  def unary_- : IndexSl = new IndexSl(bits, vs.map(-_))

  def +(that: IndexSl): IndexSl = compwise(that)(_ + _)
  def -(that: IndexSl): IndexSl = compwise(that)(_ + _)
  def *(that: IndexSl): IndexSl = compwise(that)(_ + _)
  def /(that: IndexSl): IndexSl = compwise(that)(_ + _)

  def +(s: Long): IndexSl = new IndexSl(bits, vs.map(_ + s))
  def -(s: Long): IndexSl = new IndexSl(bits, vs.map(_ - s))
  def *(s: Long): IndexSl = new IndexSl(bits, vs.map(_ * s))
  def /(s: Long): IndexSl = new IndexSl(bits, vs.map(_ / s))

  def forall(p: (Long) => Boolean): Boolean = {
    val forZero = if (bits.size <= (bits.last + 1)) p(0L) else true
    forZero && vs.forall(p)
  }

  def forall(that: IndexSl)(p: (Long, Long) => Boolean): Boolean = {
    val forZero = true // test for case of undefined in both vectors

    val at = bits.iterator
    val avt = vs.iterator
    val bt = that.bits.iterator
    val bvt = that.vs.iterator
    @tailrec def f(ai: Int, bi: Int): Boolean =
      if ((ai < bi) && at.hasNext && avt.hasNext)
        p(avt.next, 0L) && f(at.next, bi)
      else if ((bi < ai) && bt.hasNext && bvt.hasNext)
        p(0L, bvt.next) && f(ai, bt.next)
      else if (at.hasNext && avt.hasNext && bt.hasNext && bvt.hasNext)
        p(avt.next, bvt.next) && f(at.next, bt.next)
      else true

    forZero && f(-1, -1)
  }

  def equiv(that: IndexSl, epsilon: Long): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: IndexSl): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = false

  def forany(p: (Long) => Boolean): Boolean = forall(!p(_))
  def forany(that: IndexSl)(p: (Long, Long) => Boolean): Boolean = forall(that)(!p(_, _))

  def foreach(p: (Long) => Unit): Unit = vs.foreach(p)

  def map(p: (Long) => Long): IndexSl = new IndexSl(bits, vs.map(p))

  def foldLeft[A](start: A)(f: (A, Long) => A): A = vs.foldLeft(start)(f)
  def /:[A](start: A)(f: (A, Long) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Long, A) => A): A = vs.foldRight(start)(f)
  def :\[A](start: A)(f: (Long, A) => A): A = foldRight(start)(f)

  def reduce(p: (Long, Long) => Long): Long = vs.reduce(p)
  def min: Long = reduce(_ min _)
  def max: Long = reduce(_ max _)

  def compwise(that: IndexSl)(p: (Long, Long) => Long): IndexSl = {
    val nbits = bits | that.bits
    val abd = new ArrayBuilder.ofLong
    abd.sizeHint(nbits.size)

    val at = bits.iterator
    val avt = vs.iterator
    val bt = that.bits.iterator
    val bvt = that.vs.iterator
    @tailrec def f(ai: Int, bi: Int) {
      if ((ai < bi) && at.hasNext && avt.hasNext) {
        abd += p(avt.next, 0L)
        f(at.next, bi)
      }
      else if ((bi < ai) && bt.hasNext && bvt.hasNext) {
        abd += p(0L, bvt.next)
        f(ai, bt.next)
      }
      else if (at.hasNext && avt.hasNext && bt.hasNext && bvt.hasNext) {
        abd += p(avt.next, bvt.next)
        f(at.next, bt.next)
      }
    }

    f(-1, -1)
    new IndexSl(nbits, abd.result)
  }

  def min(that: IndexSl): IndexSl = compwise(that)(_ min _)
  def max(that: IndexSl): IndexSl = compwise(that)(_ max _)

  def compwise(a: IndexSl, b: IndexSl)(p: (Long, Long, Long) => Long): IndexSl = {
    throw new NotImplementedError
  }

  def clamp(lower: IndexSl, upper: IndexSl): IndexSl = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"IndexSl$toPretty"

  override def toPretty =
    "(" + (bits.toArray.zip(vs).map {
      case (i, e) => "%d:%d".format(i, e)
    }.mkString(", ")) + ")"

  def toList: List[Long] = toArray.toList

  def toArray: Array[Long] = {
    val ary = Array.fill(bits.last + 1)(0L)
    for ((i, j) <- bits.toArray.zipWithIndex) ary(i) = vs(j)
    ary
  }

  def toSparseList: List[(Int, Long)] = bits.toArray.zip(vs).toList
  def toSparseArray: Array[(Int, Long)] = bits.toArray.zip(vs)

  def getOrZero(i: Int): Long = if (i < size) this(i) else 0L

  def toVecNd: VecNd = VecNd(toArray.map(_.toDouble))
  def toVecSd: VecSd = VecSd(toSparseArray.map { case (i, e) => i -> e.toDouble })

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

object IndexSl {
  def apply(vs: Array[Long]): IndexSl = {
    val pairs = vs.zipWithIndex.filter(_._1 != 0.0).map {
      case (e, i) => i -> e
    }
    IndexSl(pairs)
  }

  def apply(pairs: Array[(Int, Long)]): IndexSl = {
    val bits = BitSet(pairs.map(_._1): _*)
    val vs = pairs.map(_._2)
    new IndexSl(bits, vs)
  }

  def apply(pairs: (Int, Long)*): IndexSl = IndexSl(pairs.toArray)
}
