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

class Index3i private (val x: Int, val y: Int, val z: Int)
    extends Tuple3[Int, Index3i]
    with Swizzle3[Index2i, Index3i]
    with TupleOps[Int, Index3i, Index3i] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Index3i =>
        (that canEqual this) && (x == that.x) && (y == that.y) && (z == that.z)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Index3i => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 47 * (43 * (41 + x.##) + y.##) + z.##

  def apply(i: Int): Int =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Int): Index3i =
    i match {
      case 0 => Index3i(e, y, z)
      case 1 => Index3i(x, e, z)
      case 2 => Index3i(x, y, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Int): Index3i = Index3i(e, y, z)
  def updateY(e: Int): Index3i = Index3i(x, e, z)
  def updateZ(e: Int): Index3i = Index3i(x, y, e)

  def xxx: Index3i = Index3i(x, x, x)
  def xxy: Index3i = Index3i(x, x, y)
  def xyx: Index3i = Index3i(x, y, x)
  def yxx: Index3i = Index3i(y, x, x)
  def xxz: Index3i = Index3i(x, x, z)
  def xzx: Index3i = Index3i(x, z, x)
  def zxx: Index3i = Index3i(z, x, x)
  def xyy: Index3i = Index3i(x, y, y)
  def yxy: Index3i = Index3i(y, x, y)
  def yyx: Index3i = Index3i(y, y, x)
  def xyz: Index3i = Index3i(x, y, z)
  def xzy: Index3i = Index3i(x, z, y)
  def yxz: Index3i = Index3i(y, x, z)
  def yzx: Index3i = Index3i(y, z, x)
  def zxy: Index3i = Index3i(z, x, y)
  def zyx: Index3i = Index3i(z, y, x)
  def xzz: Index3i = Index3i(x, z, z)
  def zxz: Index3i = Index3i(z, x, z)
  def zzx: Index3i = Index3i(z, z, x)
  def yyy: Index3i = Index3i(y, y, y)
  def yyz: Index3i = Index3i(y, y, z)
  def yzy: Index3i = Index3i(y, z, y)
  def zyy: Index3i = Index3i(z, y, y)
  def yzz: Index3i = Index3i(y, z, z)
  def zyz: Index3i = Index3i(z, y, z)
  def zzy: Index3i = Index3i(z, z, y)
  def zzz: Index3i = Index3i(z, z, z)

  def xx: Index2i = Index2i(x, x)
  def xy: Index2i = Index2i(x, y)
  def yx: Index2i = Index2i(y, x)
  def xz: Index2i = Index2i(x, z)
  def zx: Index2i = Index2i(z, x)
  def yy: Index2i = Index2i(y, y)
  def yz: Index2i = Index2i(y, z)
  def zy: Index2i = Index2i(z, y)
  def zz: Index2i = Index2i(z, z)

  def dot(that: Index3i): Int = x * that.x + y * that.y + z + that.z

  def unary_- : Index3i = Index3i(-x, -y, -z)

  def +(that: Index3i): Index3i = Index3i(x + that.x, y + that.y, z + that.z)
  def -(that: Index3i): Index3i = Index3i(x - that.x, y - that.y, z - that.z)
  def *(that: Index3i): Index3i = Index3i(x * that.x, y * that.y, z * that.z)
  def /(that: Index3i): Index3i = Index3i(x / that.x, y / that.y, z / that.z)

  def +(s: Int): Index3i = Index3i(x + s, y + s, z + s)
  def -(s: Int): Index3i = Index3i(x - s, y - s, z - s)
  def *(s: Int): Index3i = Index3i(x * s, y * s, z * s)
  def /(s: Int): Index3i = Index3i(x / s, y / s, z / s)

  def forall(p: (Int) => Boolean): Boolean = p(x) && p(y) && p(z)
  def forall(that: Index3i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x) && p(y, that.y) && p(z, that.z)
  def equiv(that: Index3i, epsilon: Int): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Index3i): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = false

  def forany(p: (Int) => Boolean): Boolean = p(x) || p(y) || p(z)
  def forany(that: Index3i)(p: (Int, Int) => Boolean): Boolean = p(x, that.x) || p(y, that.y) || p(z, that.z)

  def foreach(p: (Int) => Unit): Unit = { p(x); p(y); p(z) }

  def map(p: (Int) => Int): Index3i = Index3i(p(x), p(y), p(z))

  def foldLeft[A](start: A)(f: (A, Int) => A): A = f(f(f(start, x), y), z)
  def /:[A](start: A)(f: (A, Int) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Int, A) => A): A = f(x, f(y, f(z, start)))
  def :\[A](start: A)(f: (Int, A) => A): A = foldRight(start)(f)

  def reduce(p: (Int, Int) => Int): Int = p(x, p(y, z))
  def min: Int = reduce(_ min _)
  def max: Int = reduce(_ max _)

  def compwise(that: Index3i)(p: (Int, Int) => Int): Index3i =
    Index3i(p(x, that.x), p(y, that.y), p(z, that.z))
  def min(that: Index3i): Index3i = compwise(that)(_ min _)
  def max(that: Index3i): Index3i = compwise(that)(_ max _)

  def compwise(a: Index3i, b: Index3i)(p: (Int, Int, Int) => Int): Index3i =
    Index3i(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z))
  def clamp(lower: Index3i, upper: Index3i): Index3i = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Index3i$toPretty"

  def toPretty: String = s"($x, $y, $z)"

  def toList: List[Int] = List(x, y, z)
  def toArray: Array[Int] = Array(x, y, z)

  def toVecNd: VecNd = VecNd(toArray.map(_.toDouble))
  def toVecSd: VecSd = VecSd(toArray.map(_.toDouble))

  def toVec4d: Vec4d = Vec4d(x.toDouble, y.toDouble, z.toDouble, 0.0)
  def toVec3d: Vec3d = Vec3d(x.toDouble, y.toDouble, z.toDouble)
  def toVec2d: Vec2d = Vec2d(x.toDouble, y.toDouble)
  def toVec1d: Vec1d = Vec1d(x.toDouble)

  def toPos3d: Pos3d = Pos3d(x.toDouble, y.toDouble, z.toDouble)
  def toPos2d: Pos2d = Pos2d(x.toDouble, y.toDouble)
  def toPos1d: Pos1d = Pos1d(x.toDouble)

  def toIndex3i: Index3i = this
  def toIndex2i: Index2i = Index2i(x, y)
  def toIndex1i: Index1i = Index1i(x)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar); buf.put(z.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x); buf.put(y); buf.put(z)
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
    buf.put(x.toDouble); buf.put(y.toDouble); buf.put(z.toDouble)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Index3i {
  def apply(s: Int): Index3i = new Index3i(s, s, s)
  def apply(x: Int, y: Int, z: Int): Index3i = new Index3i(x, y, z)
  def apply(idx: Index2i, z: Int): Index3i = new Index3i(idx.x, idx.y, z)

  def apply(ls: List[Int]): Index3i =
    ls match {
      case List(x, y, z) => Index3i(x, y, z)
      case List(x, y)    => Index3i(x, y, 0)
      case List(x)       => Index3i(x, 0, 0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 3 components!")
    }

  def apply(ary: Array[Int]): Index3i =
    ary match {
      case Array(x, y, z) => Index3i(x, y, z)
      case Array(x, y)    => Index3i(x, y, 0)
      case Array(x)       => Index3i(x, 0, 0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 3 components!")
    }

  def apply(buf: CharBuffer): Index3i = Index3i(buf.get.toInt, buf.get.toInt, buf.get.toInt)
  def apply(buf: ShortBuffer): Index3i = Index3i(buf.get.toInt, buf.get.toInt, buf.get.toInt)
  def apply(buf: IntBuffer): Index3i = Index3i(buf.get, buf.get, buf.get)
  def apply(buf: LongBuffer): Index3i = Index3i(buf.get.toInt, buf.get.toInt, buf.get.toInt)
  def apply(buf: FloatBuffer): Index3i = Index3i(buf.get.toInt, buf.get.toInt, buf.get.toInt)
  def apply(buf: DoubleBuffer): Index3i = Index3i(buf.get.toInt, buf.get.toInt, buf.get.toInt)

  def unapply(a: Index3i): Some[(Int, Int, Int)] = Some((a.x, a.y, a.z))

  val zero = Index3i(0)
  val size = zero.size
}
