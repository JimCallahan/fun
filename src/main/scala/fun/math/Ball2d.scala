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

class Ball2d protected (val center: Pos2d, val radius: Double)
    extends BallAccess[Double, Pos2d, Ball2d]
    with BallOps[Double, Pos2d, BBox2d, Ball2d] {

  import scala.math.Pi

  def dimens: Int = 2

  def updateCenter(p: Pos2d): Ball2d = Ball2d(p, radius)
  def updateRadius(r: Double): Ball2d = Ball2d(center, r)

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Ball2d =>
        (that canEqual this) &&
        (center == that.center) &&
        Scalar.equiv(radius, that.radius)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Ball2d => true
      case _            => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + center.hashCode) + radius.hashCode

  def equiv(that: Ball2d, epsilon: Double): Boolean =
    center.equiv(that.center, epsilon) &&
  Scalar.equiv(radius, that.radius, epsilon)

  def equiv(that: Ball2d): Boolean =
    center.equiv(that.center) &&
  Scalar.equiv(radius, that.radius)

  def isNaN: Boolean = center.isNaN || radius.isNaN

  def area: Double = Pi * (radius * radius)

  def grow(p: Pos2d): Ball2d = {
    val v = p - center
    val r = v.mag
    if (r > radius) updateRadius(r)
    else this
  }

  def grow(v: Double): Ball2d =
    Ball2d(center, radius + v)

  def clamp(p: Pos2d): Pos2d = {
    val v = p - center
    val r = v.mag
    if (Scalar.equiv(r, 0.0)) center
    else if (r > radius) center + (v / r) * radius
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
    (r <= radius)
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
        case (Some(_), Some(_)) => true
        case (Some(d), _)       => d <= radius
        case (_, Some(d))       => d <= radius
        case _                  => ds.exists(_ <= radius)
      }
  }

  def bounds: BBox2d = BBox2d(center - Vec2d(radius), center + Vec2d(radius))

  def toBall3d: Ball3d = Ball3d(center.toPos3d, radius)
  def toBall2d: Ball2d = this
}

object Ball2d {
  /** Create a new Ball (disk) at origin, with radius one. */
  def apply(): Ball2d = new Ball2d(Pos2d(0.0), 1.0)

  /** Create a new Ball (disk). */
  def apply(center: Pos2d, radius: Double): Ball2d =
    new Ball2d(center, radius max 0.0)
}
