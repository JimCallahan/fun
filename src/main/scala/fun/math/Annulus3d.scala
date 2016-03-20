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
import scala.util.Random

class Annulus3d protected (val center: Pos3d, val inner: Double, val radius: Double)
    extends AnnulusAccess[Double, Pos3d, Annulus3d]
    with AnnulusOps[Double, Pos3d, BBox3d, Annulus3d] {

  import scala.math.Pi

  def dimens: Int = 3

  def updateCenter(p: Pos3d): Annulus3d = Annulus3d(p, inner, outer)
  def updateInner(r: Double): Annulus3d = Annulus3d(center, radius, outer)
  def updateRadius(r: Double): Annulus3d = Annulus3d(center, inner, r)

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Annulus3d =>
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
      case that: Annulus3d => true
      case _               => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    49 * (47 * (43 + center.hashCode) + inner.hashCode) + outer.hashCode

  def equiv(that: Annulus3d, epsilon: Double): Boolean = {
    center.equiv(that.center, epsilon) &&
    Scalar.equiv(inner, that.inner, epsilon) &&
    Scalar.equiv(outer, that.outer, epsilon)
  }

  def equiv(that: Annulus3d): Boolean = {
    center.equiv(that.center) &&
    Scalar.equiv(inner, that.inner) &&
    Scalar.equiv(outer, that.outer)
  }

  def isNaN: Boolean = center.isNaN || inner.isNaN || outer.isNaN

  def area: Double = (4.0 / 3.0) * Pi * ((outer * outer * outer) - (inner * inner * inner))

  def grow(p: Pos3d): Annulus3d = {
    val v = p - center
    val r = v.mag
    if (r < inner) updateInner(r)
    else if (r > outer) updateOuter(r)
    else this
  }

  def grow(dinner: Double, douter: Double): Annulus3d =
    Annulus3d(center, inner + dinner, outer + douter)

  def grow(v: Double): Annulus3d =
    Annulus3d(center, inner, outer + v)

  def width: Double = outer - inner

  def clamp(p: Pos3d): Pos3d = {
    val v = p - center
    val r = v.mag
    if (Scalar.equiv(r, 0.0)) (Vec3d.randomUnit() * inner).toPos3d
    else if (r < inner) center + (v / r) * inner
    else if (r > outer) center + (v / r) * outer
    else p
  }

  def coord(pos: Pos3d): Pos3d =
    throw new NotImplementedError

  def position(coord: Pos3d): Pos3d =
    throw new NotImplementedError

  def randomPos(): Pos3d = randomPos(new Random)
  def randomPos(gen: Random): Pos3d = {
    @tailrec
    def f: Pos3d = {
      val p = bounds.randomPos(gen)
      if (isInside(p)) p else f
    }
    f
  }

  def isInside(p: Pos3d): Boolean = {
    val v = p - center
    val r = v.mag
    (r >= inner) && (r <= outer)
  }

  def intersects(b: BBox3d): Boolean = {
    throw new NotImplementedError
  }

  def bounds: BBox3d = BBox3d(center - Vec3d(radius), center + Vec3d(outer))

  def toAnnulus3d: Annulus3d = this
  def toAnnulus2d: Annulus2d = Annulus2d(center.toPos2d, inner, outer)

  def toBall3d: Ball3d = Ball3d(center, outer)
  def toBall2d: Ball2d = Ball2d(center.toPos2d, outer)
}

object Annulus3d {
  /** Create a new Annulus (spherical shell) at origin, with inner radius zero and outer radius one. */
  def apply(): Annulus3d = new Annulus3d(Pos3d(0.0), 0.0, 1.0)

  /** Create a new Annulus (spherical shell). */
  def apply(center: Pos3d, inner: Double, outer: Double): Annulus3d =
    new Annulus3d(center, (inner min outer) max 0.0, (inner max outer) max 0.0)
}
