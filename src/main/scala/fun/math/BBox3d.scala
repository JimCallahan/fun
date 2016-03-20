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

class BBox3d protected (val bmin: Pos3d, val bmax: Pos3d)
    extends BBoxAccess[Pos3d, BBox3d]
    with BBoxOps[Double, Pos3d, Vec3d, BBox3d] {

  def dimens: Int = 3

  def corners: List[Pos3d] =
    List(
      bmin, bmin.updateX(bmax.x), bmax.updateZ(bmin.z), bmin.updateY(bmax.y),
      bmin.updateZ(bmax.z), bmax.updateY(bmin.y), bmax, bmax.updateX(bmin.x)
    )

  def apply(i: Int): Pos3d =
    i match {
      case 0 => bmin
      case 1 => bmax
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateMin(p: Pos3d): BBox3d = BBox3d(p, bmax)
  def updateMax(p: Pos3d): BBox3d = BBox3d(bmin, p)

  def update(i: Int, p: Pos3d): BBox3d =
    i match {
      case 0 => BBox3d(p, bmax)
      case 1 => BBox3d(bmin, p)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: BBox3d =>
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: BBox3d => true
      case _            => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + bmin.hashCode) + bmax.hashCode

  def equiv(that: BBox3d, epsilon: Double): Boolean =
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon)
  def equiv(that: BBox3d): Boolean =
    (bmin equiv that.bmin) && (bmax equiv that.bmax)

  def isNaN: Boolean = bmin.isNaN || bmax.isNaN

  def forall(p: (Pos3d) => Boolean): Boolean = p(bmin) && p(bmax)
  def forall(that: BBox3d)(p: (Pos3d, Pos3d) => Boolean): Boolean =
    p(bmin, that.bmin) && p(bmax, that.bmax)

  def forany(p: (Pos3d) => Boolean): Boolean = p(bmin) || p(bmax)
  def forany(that: BBox3d)(p: (Pos3d, Pos3d) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  def foreach(p: (Pos3d) => Unit): Unit = { p(bmin); p(bmax) }

  def map(p: (Pos3d) => Pos3d): BBox3d = BBox3d(p(bmin), p(bmax))

  def reduce(p: (Pos3d, Pos3d) => Pos3d): Pos3d = p(bmin, bmax)

  def compwise(that: BBox3d)(p: (Pos3d, Pos3d) => Pos3d): BBox3d =
    BBox3d(p(bmin, that.bmin), p(bmax, that.bmax))
  def min(that: BBox3d): BBox3d = compwise(that)(_ min _)
  def max(that: BBox3d): BBox3d = compwise(that)(_ max _)

  def area: Double = (1.0 /: range)(_ * _)

  def grow(p: Pos3d): BBox3d = BBox3d(p min bmin, p max bmax)
  def grow(v: Vec3d): BBox3d = BBox3d(bmin - v, bmax + v)

  def union(that: BBox3d): BBox3d =
    BBox3d(bmin min that.bmin, bmax max that.bmax)

  def range: Vec3d = bmax - bmin
  def center: Pos3d = bmin.lerp(bmax, 0.5)

  def clamp(p: Pos3d): Pos3d = (p max bmin) min bmax

  def coord(pos: Pos3d): Pos3d = ((pos - bmin) * (Vec3d(1.0) / range)).toPos3d
  def position(coord: Pos3d): Pos3d = bmin + (coord * range).toVec3d

  def split(dimen: Int): (BBox3d, BBox3d) = {
    val c = center
    (updateMax(bmax.update(dimen, c(dimen))), updateMin(bmin.update(dimen, c(dimen))))
  }
  def splitX: (BBox3d, BBox3d) = split(0)
  def splitY: (BBox3d, BBox3d) = split(1)
  def splitZ: (BBox3d, BBox3d) = split(2)

  def split(dimen: Int, coord: Double): (BBox3d, BBox3d) = {
    val s = position(Pos3d(coord))(dimen)
    (updateMax(bmax.update(dimen, s)), updateMin(bmin.update(dimen, s)))
  }
  def splitX(coord: Double): (BBox3d, BBox3d) = split(0, coord)
  def splitY(coord: Double): (BBox3d, BBox3d) = split(1, coord)
  def splitZ(coord: Double): (BBox3d, BBox3d) = split(2, coord)

  def randomPos(): Pos3d = position(Pos3d(Random.nextDouble, Random.nextDouble, Random.nextDouble))
  def randomPos(gen: Random): Pos3d = position(Pos3d(gen.nextDouble, gen.nextDouble, gen.nextDouble))

  def isInside(p: Pos3d): Boolean = p.forall(bmin)(_ >= _) && p.forall(bmax)(_ <= _)
  def intersection(that: BBox3d): Option[BBox3d] = {
    val mn = bmin max that.bmin
    val mx = bmax min that.bmax
    if (!mn.forany(mx)(_ > _)) Some(BBox3d(mn, mx)) else None
  }
  def intersects(that: BBox3d): Boolean = !((bmin max that.bmin).forany(bmax min that.bmax)(_ > _))

  /** Convert to a String representation */
  override def toString = s"BBox3d$toPretty"

  def toPretty: String = s"(${bmin.toPretty}, ${bmax.toPretty})"

  def toBBox3d: BBox3d = this
  def toBBox2d: BBox2d = BBox2d(bmin.toPos2d, bmax.toPos2d)
  def toBBox1d: BBox1d = BBox1d(bmin.toPos1d, bmax.toPos1d)
}

object BBox3d {
  /** Create a new bounding box from [0, 1] in all dimensions. */
  def apply(): BBox3d = new BBox3d(Pos3d(0.0), Pos3d(1.0))

  /** Create a new bounding box given two corners. */
  def apply(bmin: Pos3d, bmax: Pos3d): BBox3d = new BBox3d(bmin min bmax, bmin max bmax)

  def unapply(a: BBox3d): Some[(Pos3d, Pos3d)] = Some((a.bmin, a.bmax))

  /** Create a new bounding box which contains the given points. */
  def apply(pts: Pos3d*): BBox3d =
    if (pts.isEmpty) BBox3d(Pos3d.origin, Pos3d.origin)
    else pts.toArray.splitAt(1) match {
      case (Array(hd), tl) => new BBox3d((hd /: tl)(_ min _), (hd /: tl)(_ max _))
    }
}
