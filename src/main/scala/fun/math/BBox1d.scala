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

import scala.util.Random

class BBox1d protected (val bmin: Pos1d, val bmax: Pos1d)
    extends BBoxAccess[Pos1d, BBox1d]
    with BBoxOps[Double, Pos1d, Vec1d, BBox1d] {

  def dimens: Int = 1

  def corners: List[Pos1d] = List(bmin, bmax)

  def apply(i: Int): Pos1d =
    i match {
      case 0 => bmin
      case 1 => bmax
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateMin(p: Pos1d): BBox1d = BBox1d(p, bmax)
  def updateMax(p: Pos1d): BBox1d = BBox1d(bmin, p)

  def update(i: Int, p: Pos1d): BBox1d =
    i match {
      case 0 => BBox1d(p, bmax)
      case 1 => BBox1d(bmin, p)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: BBox1d =>
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: BBox1d => true
      case _            => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + bmin.hashCode) + bmax.hashCode

  def equiv(that: BBox1d, epsilon: Double): Boolean =
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon)
  def equiv(that: BBox1d): Boolean =
    (bmin equiv that.bmin) && (bmax equiv that.bmax)

  def isNaN: Boolean = bmin.isNaN || bmax.isNaN

  def forall(p: (Pos1d) => Boolean): Boolean = p(bmin) && p(bmax)
  def forall(that: BBox1d)(p: (Pos1d, Pos1d) => Boolean): Boolean =
    p(bmin, that.bmin) && p(bmax, that.bmax)

  def forany(p: (Pos1d) => Boolean): Boolean = p(bmin) || p(bmax)
  def forany(that: BBox1d)(p: (Pos1d, Pos1d) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  def foreach(p: (Pos1d) => Unit): Unit = { p(bmin); p(bmax) }

  def map(p: (Pos1d) => Pos1d): BBox1d = BBox1d(p(bmin), p(bmax))

  def reduce(p: (Pos1d, Pos1d) => Pos1d): Pos1d = p(bmin, bmax)

  def compwise(that: BBox1d)(p: (Pos1d, Pos1d) => Pos1d): BBox1d =
    BBox1d(p(bmin, that.bmin), p(bmax, that.bmax))
  def min(that: BBox1d): BBox1d = compwise(that)(_ min _)
  def max(that: BBox1d): BBox1d = compwise(that)(_ max _)

  def area: Double = (1.0 /: range)(_ * _)

  def grow(p: Pos1d): BBox1d = BBox1d(p min bmin, p max bmax)
  def grow(v: Vec1d): BBox1d = BBox1d(bmin - v, bmax + v)

  def union(that: BBox1d): BBox1d =
    BBox1d(bmin min that.bmin, bmax max that.bmax)

  def range: Vec1d = bmax - bmin
  def center: Pos1d = bmin.lerp(bmax, 0.5)

  def clamp(p: Pos1d): Pos1d = (p max bmin) min bmax

  def coord(pos: Pos1d): Pos1d = ((pos - bmin) * (Vec1d(1.0) / range)).toPos1d
  def position(coord: Pos1d): Pos1d = bmin + (coord * range).toVec1d

  def split(dimen: Int): (BBox1d, BBox1d) = {
    val c = center
    (updateMax(bmax.update(dimen, c(dimen))), updateMin(bmin.update(dimen, c(dimen))))
  }
  def splitX: (BBox1d, BBox1d) = split(0)

  def split(dimen: Int, coord: Double): (BBox1d, BBox1d) = {
    val s = position(Pos1d(coord))(dimen)
    (updateMax(bmax.update(dimen, s)), updateMin(bmin.update(dimen, s)))
  }
  def splitX(coord: Double): (BBox1d, BBox1d) = split(0, coord)

  def randomPos(): Pos1d = position(Pos1d(Random.nextDouble))
  def randomPos(gen: Random): Pos1d = position(Pos1d(gen.nextDouble))

  def isInside(p: Pos1d): Boolean = p.forall(bmin)(_ >= _) && p.forall(bmax)(_ <= _)
  def intersection(that: BBox1d): Option[BBox1d] = {
    val mn = bmin max that.bmin
    val mx = bmax min that.bmax
    if (!mn.forany(mx)(_ > _)) Some(BBox1d(mn, mx)) else None
  }
  def intersects(that: BBox1d): Boolean = !((bmin max that.bmin).forany(bmax min that.bmax)(_ > _))

  /** Convert to a String representation */
  override def toString = s"BBox1d$toPretty"

  def toPretty: String = s"(${bmin.toPretty}, ${bmax.toPretty})"

  def toBBox3d: BBox3d = BBox3d(bmin.toPos3d, bmax.toPos3d)
  def toBBox2d: BBox2d = BBox2d(bmin.toPos2d, bmax.toPos2d)
  def toBBox1d: BBox1d = this

  /** Convert to an Interval. */
  def toInterval: Interval[Double] = Interval(bmin.x, bmax.x)
}

object BBox1d {
  /** Create a new bounding box from [0, 1] in all dimensions. */
  def apply(): BBox1d = new BBox1d(Pos1d(0.0), Pos1d(1.0))

  /** Create a new bounding box given two corners. */
  def apply(bmin: Pos1d, bmax: Pos1d): BBox1d = new BBox1d(bmin min bmax, bmin max bmax)

  def unapply(a: BBox1d): Some[(Pos1d, Pos1d)] = Some((a.bmin, a.bmax))

  /** Create a new bounding box which contains the given points. */
  def apply(pts: Pos1d*): BBox1d =
    if (pts.isEmpty) BBox1d(Pos1d.origin, Pos1d.origin)
    else pts.toArray.splitAt(1) match {
      case (Array(hd), tl) => new BBox1d((hd /: tl)(_ min _), (hd /: tl)(_ max _))
    }
}
