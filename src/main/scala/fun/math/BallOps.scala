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

trait BallOps[@specialized(Double) Elem, P <: Pos, B <: BBox, Repr <: Ball]
    extends Numeric {

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean

  /** Whether another Ball's components are all within a given epsilon of this Ball. */
  def equiv(that: Repr, epsilon: Elem): Boolean

  /** Whether another Ball's components are all within a the standard epsilon of this Ball. */
  def equiv(that: Repr): Boolean

  /** The area (or volume) enclosed. */
  def area: Elem

  /** Create a Ball which grows the current radius to contain the given position. */
  def grow(p: P): Repr

  /** Create a Ball which grows the current radius by the given amounts. */
  def grow(v: Double): Repr

  /** Clamp the given point to the bounds of the Ball by projecting it along the vector from the center
    * until it meets the radius.
    */
  def clamp(p: P): P

  /** The local polar coordinates of a world position. */
  def coord(pos: P): P

  /** The world position of a local polar coordinate. */
  def position(coord: P): P

  /** Generate a random position inside the Ball. */
  def randomPos(): P

  /** Generate a random position inside the Ball.
    * @param gen The random number generator to use.
    */
  def randomPos(gen: Random): P

  /** Whether the given point in inside of the Ball. */
  def isInside(p: P): Boolean

  /** Whether the given bounding box shares any volume with this Ball. */
  def intersects(b: B): Boolean

  /** The bounding box containing the Ball. */
  def bounds: B

  /** Convert to a 3-dimensional Ball (spherical shell). */
  def toBall3d: Ball3d

  /** Convert to a 2-dimensional Ball (ring). */
  def toBall2d: Ball2d
}
