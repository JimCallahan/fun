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

class Ball3d protected (val center: Pos3d, val radius: Double)
    extends BallAccess[Double, Pos3d, Ball3d]
    with BallOps[Double, Pos3d, BBox3d, Ball3d] {

  import scala.math.Pi

  def dimens: Int = 3

  def updateCenter(p: Pos3d): Ball3d = Ball3d(p, radius)
  def updateRadius(r: Double): Ball3d = Ball3d(center, r)

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Ball3d =>
        (that canEqual this) &&
        (center == that.center) &&
        Scalar.equiv(radius, that.radius)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Ball3d => true
      case _            => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + center.hashCode) + radius.hashCode

  def equiv(that: Ball3d, epsilon: Double): Boolean =
    center.equiv(that.center, epsilon) &&
  Scalar.equiv(radius, that.radius, epsilon)

  def equiv(that: Ball3d): Boolean =
    center.equiv(that.center) &&
  Scalar.equiv(radius, that.radius)

  def isNaN: Boolean = center.isNaN || radius.isNaN

  def area: Double = (4.0 / 3.0) * Pi * (radius * radius * radius)

  def grow(p: Pos3d): Ball3d = {
    val v = p - center
    val r = v.mag
    if (r > radius) updateRadius(r)
    else this
  }

  def grow(v: Double): Ball3d =
    Ball3d(center, radius + v)

  def clamp(p: Pos3d): Pos3d = {
    val v = p - center
    val r = v.mag
    if (Scalar.equiv(r, 0.0)) center
    else if (r > radius) center + (v / r) * radius
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
    (r <= radius)
  }

  def intersects(b: BBox3d): Boolean = {

    throw new NotImplementedError
  }

  def bounds: BBox3d = BBox3d(center - Vec3d(radius), center + Vec3d(radius))

  def toBall3d: Ball3d = this
  def toBall2d: Ball2d = Ball2d(center.toPos2d, radius)
}

object Ball3d {
  /** Create a new Ball (spherical volume) at origin, with radius one. */
  def apply(): Ball3d = new Ball3d(Pos3d(0.0), 1.0)

  /** Create a new Ball (spherical volume). */
  def apply(center: Pos3d, radius: Double): Ball3d =
    new Ball3d(center, radius max 0.0)
}
