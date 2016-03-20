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

trait BBoxOps[@specialized(Double, Int) Elem, P <: Pos, V <: Vec, Repr <: BBox]
    extends Numeric {

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean

  /** Whether another bounding box is within a given epsilon of this bounding box. */
  def equiv(that: Repr, epsilon: Elem): Boolean

  /** Whether another bounding box is within a the standard epsilon of this bounding box. */
  def equiv(that: Repr): Boolean

  /** Tests whether a predicate holds true for both corners this bounding box. */
  def forall(p: (P) => Boolean): Boolean

  /** Tests whether a predicate holds true for the corresponding corners
    * of this and another bounding box. */
  def forall(that: Repr)(p: (P, P) => Boolean): Boolean

  /** Tests whether a predicate holds true for either corner of this bounding box. */
  def forany(p: (P) => Boolean): Boolean

  /** Tests whether a predicate holds true for either of the corresponding corners
    * of this and another bounding box. */
  def forany(that: Repr)(p: (P, P) => Boolean): Boolean

  /** Applies a function to both corners of this bounding box.
    * @param p The function that is applied for its side-effect only each corner. */
  def foreach(p: (P) => Unit): Unit

  /** Builds a new bounding box by applying a function to each corner of this bounding box. */
  def map(p: (P) => P): Repr

  /** Reduces the corners of this bounding box using the specified associative binary operator. */
  def reduce(p: (P, P) => P): P

  /** Builds a new bounding box by apply a function to the corresponding corners of this and another
    * bounding box. */
  def compwise(that: Repr)(p: (P, P) => P): Repr

  /** Builds a new bounding box who's corners are the minimum of the corresponding corners of this
    * and another bounding box. */
  def min(that: Repr): Repr

  /** Builds a new bounding box who's corners are the maximum of the corresponding corners of this
    * and another bounding box. */
  def max(that: Repr): Repr

  /** The area enclosed by the bounding box. */
  def area: Elem

  /** Create a bounding box which grows the current bounding box to contain the given position. */
  def grow(p: P): Repr

  /** Create a bounding box which grows (or shrinks) the current bounding box by the given amount. */
  def grow(v: V): Repr

  /** Create a bounding box which contains the volume of both this and the given bounding box. */
  def union(that: Repr): Repr

  /** The range of the bounding box. */
  def range: V

  /** The center of the bounding box. */
  def center: P

  /** Clamp the given point to the bounds of the box. */
  def clamp(p: P): P

  /** The local coordinate of a world position. */
  def coord(pos: P): P

  /** The world position of a local coordinate. */
  def position(coord: P): P

  /** Split a bounding box along the midpoint of the given dimension. */
  def split(dimen: Int): (Repr, Repr)

  /** Split a bounding box along the coordinate [0, 1) of the given dimension. */
  def split(dimen: Int, coord: Double): (Repr, Repr)

  /** Generate a random position inside the bounding box. */
  def randomPos(): P

  /** Generate a random position inside the bounding box.
    * @param gen The random number generator to use. */
  def randomPos(gen: Random): P

  /** Whether the given point in inside of the bounding box. */
  def isInside(p: P): Boolean

  /** The bounding box of the region shared with the given bounding box, if any. */
  def intersection(that: Repr): Option[Repr]

  /** Whether the given bounding box shares any volume with this bounding box. */
  def intersects(that: Repr): Boolean

  /** Convert to a 3-dimensional bounding box. */
  def toBBox3d: BBox3d

  /** Convert to a 2-dimensional bounding box. */
  def toBBox2d: BBox2d

  /** Convert to a 1-dimensional bounding box. */
  def toBBox1d: BBox1d

  /** Convert to a more compact String representation. */
  def toPretty: String
}
