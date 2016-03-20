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

trait SectorOps[@specialized(Double) Elem, P <: Pos, V <: Vec, B <: BBox, Repr <: Sector]
    extends AnnulusOps[Elem, P, B, Repr] {

  /** Whether another Sector's components are all within a given epsilon of this Sector. */
  def equiv(that: Repr, epsilon: Elem): Boolean

  /** Whether another Sector's components are all within a the standard epsilon of this Sector. */
  def equiv(that: Repr): Boolean

  /** Create a Sector which grows (or shrinks) the current radii and edge vectors to contain the
    * given position. */
  def grow(p: P): Repr

  /** Create a Sector which grows the current edge vectors contain the given vectors. */
  def grow(v: V): Repr

  /** Create a Sector which grows (or shrinks) the current radii by the given amounts. */
  def grow(inner: Elem, outer: Elem): Repr

  /** Clamp the given point to the bounds of the Sector by projecting it along the vector from the center
    * until it meets the nearer of the inner/outer radii, if this is outside the edge vectors project along
    * the radius to the nearest edge. */
  def clamp(p: P): P

  /** The clockwise angle (in radians) between the left and right edge vectors. */
  def angle: Double

  /** Generate a random position inside the Sector. */
  def randomPos(): P

  /** Generate a random position inside the Sector.
    * @param gen The random number generator to use.
    */
  def randomPos(gen: Random): P

  /** Whether the given point in inside of the Sector. */
  def isInside(p: P): Boolean
}
