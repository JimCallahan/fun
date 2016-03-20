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
import scala.annotation.tailrec

class Annulus2d protected (val center: Pos2d, val inner: Double, val radius: Double)
    extends AnnulusAccess[Double, Pos2d, Annulus2d]
    with AnnulusOps[Double, Pos2d, BBox2d, Annulus2d] {

  import scala.math.Pi

  def dimens: Int = 2

  def updateCenter(p: Pos2d): Annulus2d = Annulus2d(p, inner, outer)
  def updateInner(r: Double): Annulus2d = Annulus2d(center, radius, outer)
  def updateRadius(r: Double): Annulus2d = Annulus2d(center, inner, r)

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Annulus2d =>
        (that canEqual this) &&
        (center == that.center) &&
        Scalar.equiv(inner, that.inner) &&
        Scalar.equiv(outer, that.outer)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Annulus2d => true
      case _               => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    49 * (47 * (43 + center.hashCode) + inner.hashCode) + outer.hashCode

  def equiv(that: Annulus2d, epsilon: Double): Boolean = {
    center.equiv(that.center, epsilon) &&
    Scalar.equiv(inner, that.inner, epsilon) &&
    Scalar.equiv(outer, that.outer, epsilon)
  }

  def equiv(that: Annulus2d): Boolean = {
    center.equiv(that.center) &&
    Scalar.equiv(inner, that.inner) &&
    Scalar.equiv(outer, that.outer)
  }

  def isNaN: Boolean = center.isNaN || inner.isNaN || outer.isNaN

  def area: Double = Pi * ((outer * outer) - (inner * inner))

  def grow(p: Pos2d): Annulus2d = {
    val v = p - center
    val r = v.mag
    if (r < inner) updateInner(r)
    else if (r > outer) updateOuter(r)
    else this
  }

  def grow(dinner: Double, douter: Double): Annulus2d =
    Annulus2d(center, inner + dinner, outer + douter)

  def grow(v: Double): Annulus2d =
    Annulus2d(center, inner, outer + v)

  def width: Double = outer - inner

  def clamp(p: Pos2d): Pos2d = {
    val v = p - center
    val r = v.mag
    if (Scalar.equiv(r, 0.0)) (Vec2d.randomUnit() * inner).toPos2d
    else if (r < inner) center + (v / r) * inner
    else if (r > outer) center + (v / r) * outer
    else p
  }

  def coord(pos: Pos2d): Pos2d =
    throw new NotImplementedError

  def position(coord: Pos2d): Pos2d =
    throw new NotImplementedError

  def randomPos(): Pos2d = randomPos(new Random)
  def randomPos(gen: Random): Pos2d = {
    @tailrec
    def f: Pos2d = {
      val p = bounds.randomPos(gen)
      if (isInside(p)) p else f
    }
    f
  }

  def isInside(p: Pos2d): Boolean = {
    val v = p - center
    val r = v.mag
    (r >= inner) && (r <= outer)
  }

  def intersects(b: BBox2d): Boolean = {
    val BBox2d(bmin, bmax) = b
    val ds = b.corners map (p => (p - center).mag)
    val cx =
      if ((center.y < bmin.y) || (center.y > bmax.y)) None
      else Some((bmin.x - center.x) max (center.x - bmax.x))
    val cy =
      if ((center.x < bmin.x) || (center.x > bmax.x)) None
      else Some((bmin.y - center.y) max (center.y - bmax.y))
        (cx, cy) match {
        case (Some(_), Some(_)) => ds.exists(_ >= inner)
        case (Some(d), _)       => d <= outer
        case (_, Some(d))       => d <= outer
        case _                  => ds.exists(_ <= outer) && !ds.forall(_ <= inner)
      }
  }

  def bounds: BBox2d = BBox2d(center - Vec2d(outer), center + Vec2d(outer))

  def toAnnulus3d: Annulus3d = Annulus3d(center.toPos3d, inner, outer)
  def toAnnulus2d: Annulus2d = this

  def toBall3d: Ball3d = Ball3d(center.toPos3d, outer)
  def toBall2d: Ball2d = Ball2d(center, outer)
}

object Annulus2d {
  /** Create a new Annulus (ring) at origin, with inner radius zero and outer radius one. */
  def apply(): Annulus2d = new Annulus2d(Pos2d(0.0), 0.0, 1.0)

  /** Create a new Annulus (ring). */
  def apply(center: Pos2d, inner: Double, outer: Double): Annulus2d =
    new Annulus2d(center, (inner min outer) max 0.0, (inner max outer) max 0.0)
}
