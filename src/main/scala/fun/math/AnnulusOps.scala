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

trait AnnulusOps[@specialized(Double) Elem, P <: Pos, B <: BBox, Repr <: Annulus]
    extends BallOps[Elem, P, B, Repr] {

  /** Whether another Annulus's components are all within a given epsilon of this Annulus. */
  def equiv(that: Repr, epsilon: Elem): Boolean

  /** Whether another Annulus's components are all within a the standard epsilon of this Annulus. */
  def equiv(that: Repr): Boolean

  /** Create a Annulus which grows (or shrinks) the current radii to contain the given position. */
  def grow(p: P): Repr

  /** Create a Annulus which grows (or shrinks) the current radii by the given amounts. */
  def grow(inner: Elem, outer: Elem): Repr

  /** The distance between the inner and outer radii. */
  def width: Elem

  /** Clamp the given point to the bounds of the Annulus by projecting it along the vector from the center
    * until it meets the nearer of the inner/outer radii.
    */
  def clamp(p: P): P

  /** Generate a random position inside the Annulus. */
  def randomPos(): P

  /** Generate a random position inside the Annulus.
    * @param gen The random number generator to use.
    */
  def randomPos(gen: Random): P

  /** Whether the given point in inside of the Annulus. */
  def isInside(p: P): Boolean

  /** Whether the given bounding box shares any volume with this Annulus. */
  def intersects(b: B): Boolean

  /** Convert to a 3-dimensional Annulus (spherical shell). */
  def toAnnulus3d: Annulus3d

  /** Convert to a 2-dimensional Annulus (ring). */
  def toAnnulus2d: Annulus2d
}
