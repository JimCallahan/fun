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

class BBox2d protected (val bmin: Pos2d, val bmax: Pos2d)
    extends BBoxAccess[Pos2d, BBox2d]
    with BBoxOps[Double, Pos2d, Vec2d, BBox2d] {

  def dimens: Int = 2

  def corners: List[Pos2d] = List(bmin, bmin.updateX(bmax.x), bmax, bmax.updateX(bmin.x))

  def apply(i: Int): Pos2d =
    i match {
      case 0 => bmin
      case 1 => bmax
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateMin(p: Pos2d): BBox2d = BBox2d(p, bmax)
  def updateMax(p: Pos2d): BBox2d = BBox2d(bmin, p)

  def update(i: Int, p: Pos2d): BBox2d =
    i match {
      case 0 => BBox2d(p, bmax)
      case 1 => BBox2d(bmin, p)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: BBox2d =>
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: BBox2d => true
      case _            => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + bmin.hashCode) + bmax.hashCode

  def equiv(that: BBox2d, epsilon: Double): Boolean =
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon)
  def equiv(that: BBox2d): Boolean =
    (bmin equiv that.bmin) && (bmax equiv that.bmax)

  def isNaN: Boolean = bmin.isNaN || bmax.isNaN

  def forall(p: (Pos2d) => Boolean): Boolean = p(bmin) && p(bmax)
  def forall(that: BBox2d)(p: (Pos2d, Pos2d) => Boolean): Boolean =
    p(bmin, that.bmin) && p(bmax, that.bmax)

  def forany(p: (Pos2d) => Boolean): Boolean = p(bmin) || p(bmax)
  def forany(that: BBox2d)(p: (Pos2d, Pos2d) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  def foreach(p: (Pos2d) => Unit): Unit = { p(bmin); p(bmax) }

  def map(p: (Pos2d) => Pos2d): BBox2d = BBox2d(p(bmin), p(bmax))

  def reduce(p: (Pos2d, Pos2d) => Pos2d): Pos2d = p(bmin, bmax)

  def compwise(that: BBox2d)(p: (Pos2d, Pos2d) => Pos2d): BBox2d =
    BBox2d(p(bmin, that.bmin), p(bmax, that.bmax))
  def min(that: BBox2d): BBox2d = compwise(that)(_ min _)
  def max(that: BBox2d): BBox2d = compwise(that)(_ max _)

  def area: Double = (1.0 /: range)(_ * _)

  def grow(p: Pos2d): BBox2d = BBox2d(p min bmin, p max bmax)
  def grow(v: Vec2d): BBox2d = BBox2d(bmin - v, bmax + v)

  def union(that: BBox2d): BBox2d =
    BBox2d(bmin min that.bmin, bmax max that.bmax)

  def range: Vec2d = bmax - bmin
  def center: Pos2d = bmin.lerp(bmax, 0.5)

  def clamp(p: Pos2d): Pos2d = (p max bmin) min bmax

  def coord(pos: Pos2d): Pos2d = ((pos - bmin) * (Vec2d(1.0) / range)).toPos2d
  def position(coord: Pos2d): Pos2d = bmin + (coord * range).toVec2d

  def split(dimen: Int): (BBox2d, BBox2d) = {
    val c = center
    (updateMax(bmax.update(dimen, c(dimen))), updateMin(bmin.update(dimen, c(dimen))))
  }
  def splitX: (BBox2d, BBox2d) = split(0)
  def splitY: (BBox2d, BBox2d) = split(1)

  def split(dimen: Int, coord: Double): (BBox2d, BBox2d) = {
    val s = position(Pos2d(coord))(dimen)
    (updateMax(bmax.update(dimen, s)), updateMin(bmin.update(dimen, s)))
  }
  def splitX(coord: Double): (BBox2d, BBox2d) = split(0, coord)
  def splitY(coord: Double): (BBox2d, BBox2d) = split(1, coord)

  def randomPos(): Pos2d = position(Pos2d(Random.nextDouble, Random.nextDouble))
  def randomPos(gen: Random): Pos2d = position(Pos2d(gen.nextDouble, gen.nextDouble))

  def isInside(p: Pos2d): Boolean = p.forall(bmin)(_ >= _) && p.forall(bmax)(_ <= _)
  def intersection(that: BBox2d): Option[BBox2d] = {
    val mn = bmin max that.bmin
    val mx = bmax min that.bmax
    if (!mn.forany(mx)(_ > _)) Some(BBox2d(mn, mx)) else None
  }
  def intersects(that: BBox2d): Boolean = !((bmin max that.bmin).forany(bmax min that.bmax)(_ > _))

  /** Convert to a String representation */
  override def toString = s"BBox2d$toPretty"

  def toPretty: String = s"(${bmin.toPretty}, ${bmax.toPretty})"


  def toBBox3d: BBox3d = BBox3d(bmin.toPos3d, bmax.toPos3d)
  def toBBox2d: BBox2d = this
  def toBBox1d: BBox1d = BBox1d(bmin.toPos1d, bmax.toPos1d)
}

object BBox2d {
  /** Create a new bounding box from [0, 1] in all dimensions. */
  def apply(): BBox2d = new BBox2d(Pos2d(0.0), Pos2d(1.0))

  /** Create a new bounding box given two corners. */
  def apply(bmin: Pos2d, bmax: Pos2d): BBox2d = new BBox2d(bmin min bmax, bmin max bmax)

  def unapply(a: BBox2d): Some[(Pos2d, Pos2d)] = Some((a.bmin, a.bmax))

  /** Create a new bounding box which contains the given points. */
  def apply(pts: Pos2d*): BBox2d =
    if (pts.isEmpty) BBox2d(Pos2d.origin, Pos2d.origin)
    else pts.toArray.splitAt(1) match {
      case (Array(hd), tl) => new BBox2d((hd /: tl)(_ min _), (hd /: tl)(_ max _))
    }
}
