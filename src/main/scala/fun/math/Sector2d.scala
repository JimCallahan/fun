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

class Sector2d protected (
  val center: Pos2d,
  val inner: Double,
  val radius: Double,
  val left: Vec2d,
  val right: Vec2d
)
    extends SectorAccess[Double, Pos2d, Vec2d, Sector2d]
    with SectorOps[Double, Pos2d, Vec2d, BBox2d, Sector2d] {

  import scala.math.{ Pi, acos }

  def dimens: Int = 2

  def updateCenter(p: Pos2d): Sector2d = Sector2d(p, inner, outer, left, right)
  def updateInner(r: Double): Sector2d = Sector2d(center, r, outer, left, right)
  def updateRadius(r: Double): Sector2d = Sector2d(center, inner, r, left, right)
  def updateLeft(v: Vec2d): Sector2d = Sector2d(center, inner, radius, v.normalized, right)
  def updateRight(v: Vec2d): Sector2d = Sector2d(center, inner, radius, left, v.normalized)

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Sector2d =>
        (that canEqual this) &&
        (center == that.center) &&
        Scalar.equiv(inner, that.inner) &&
        Scalar.equiv(outer, that.outer) &&
        (left == that.left) &&
        (right == that.right)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Sector2d => true
      case _              => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    53 * (51 * (49 * (47 * (43 + center.hashCode) + inner.hashCode) + outer.hashCode) + left.hashCode) + right.hashCode

  def equiv(that: Sector2d, epsilon: Double): Boolean = {
    center.equiv(that.center, epsilon) &&
    Scalar.equiv(inner, that.inner, epsilon) &&
    Scalar.equiv(outer, that.outer, epsilon) &&
    left.equiv(that.left, epsilon) &&
    right.equiv(that.right, epsilon)
  }

  def equiv(that: Sector2d): Boolean = {
    center.equiv(that.center) &&
    Scalar.equiv(inner, that.inner) &&
    Scalar.equiv(outer, that.outer) &&
    left.equiv(that.left) &&
    right.equiv(that.right)
  }

  def isNaN: Boolean = center.isNaN || inner.isNaN || outer.isNaN || left.isNaN || right.isNaN

  def area: Double = (acos(left dot right) / 2.0) * ((outer * outer) - (inner * inner))

  def grow(p: Pos2d): Sector2d = {
    val v = p - center
    val r = v.mag
    val (i, o) =
      if (r < inner) (r, outer)
      else if (r > outer) (inner, r)
      else (inner, outer)
    Sector2d(center, i, o, left, right).grow(v)
  }

  def grow(v: Vec2d): Sector2d = {
    val r = v.mag
    if (Scalar.equiv(r, 0.0)) this
    else {
      val vn = v / r
      val ang = angle
      if (Vec2d.clockwise(vn, right) > angle) Sector2d(center, inner, outer, vn, right)
      else if (Vec2d.clockwise(left, vn) > angle) Sector2d(center, inner, outer, left, vn)
      else this
    }
  }

  def grow(dinner: Double, douter: Double): Sector2d =
    Sector2d(center, inner + dinner, outer + douter, left, right)

  def grow(v: Double): Sector2d =
    Sector2d(center, inner, outer + v, left, right)

  def width: Double = outer - inner

  /** The clockwise angle (in radians) between the left and right edge vectors. */
  def angle: Double = Vec2d.clockwise(left, right)

  def clamp(p: Pos2d): Pos2d = {
    val v = p - center
    val r = v.mag
    if (Scalar.equiv(r, 0.0)) (Vec2d.randomUnit() * inner).toPos2d
    else {
      val vn = v / r
      val ang = angle
      val cvn =
        if (Vec2d.clockwise(vn, right) > angle) left
        else if (Vec2d.clockwise(left, vn) > angle) right
        else vn
      if (r < inner) center + cvn * inner
      else if (r > outer) center + cvn * outer
      else (cvn * r).toPos2d
    }
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
    if ((r < inner) || (r > outer)) false
    else {
      if (Scalar.equiv(v.mag, 0.0)) true
      val vn = v / r
      val ang = angle
        (Vec2d.clockwise(vn, right) <= angle) && (Vec2d.clockwise(left, vn) <= angle)
    }
  }

  def intersects(b: BBox2d): Boolean = {
    if (!toAnnulus2d.intersects(b)) false
    else if (b.corners.exists(isInside(_))) true
    else {
      // Brute force method, probably something more elegant possible.
      @tailrec
      def f(i: Int): Boolean = {
        if (i == 0) false
        else if (isInside(b.randomPos)) true
        else f(i - 1)
      }
      f(200)
    }
  }

  def bounds: BBox2d = BBox2d(center - Vec2d(radius), center + Vec2d(outer))

  def toAnnulus3d: Annulus3d = Annulus3d(center.toPos3d, inner, outer)
  def toAnnulus2d: Annulus2d = Annulus2d(center, inner, outer)

  def toBall3d: Ball3d = Ball3d(center.toPos3d, outer)
  def toBall2d: Ball2d = Ball2d(center, outer)
}

object Sector2d {
  /** Create a new Sector (ring) at origin, with inner radius zero, outer radius one. */
  def apply(): Sector2d =
    new Sector2d(Pos2d(0.0), 0.0, 1.0, Vec2d.unitY, Vec2d.unitX)

  /** Create a new Sector (ring). */
  def apply(center: Pos2d, inner: Double, outer: Double, left: Vec2d, right: Vec2d): Sector2d =
    new Sector2d(
      center,
      (inner min outer) max 0.0,
      (inner max outer) max 0.0,
      left.normalized,
      right.normalized
    )
}
