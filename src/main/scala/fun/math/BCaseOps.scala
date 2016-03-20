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

trait BCaseOps[@specialized(Int, Long) Elem, I <: Tuple, Repr <: BCase] {

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean

  /** Tests whether a predicate holds true for both corners this bounding box. */
  def forall(p: (I) => Boolean): Boolean

  /** Tests whether a predicate holds true for the corresponding corners
    * of this and another bounding box.
    */
  def forall(that: Repr)(idx: (I, I) => Boolean): Boolean

  /** Tests whether a predicate holds true for either corner of this bounding box. */
  def forany(p: (I) => Boolean): Boolean

  /** Tests whether a predicate holds true for either of the corresponding corners
    * of this and another bounding box.
    */
  def forany(that: Repr)(idx: (I, I) => Boolean): Boolean

  /** Applies a function to both corners of this bounding box.
    * @param op The function that is applied for its side-effect only each corner.
    */
  def foreach(p: (I) => Unit): Unit

  /** Builds a new bounding box by applying a function to each corner of this bounding box. */
  def map(p: (I) => I): Repr

  /** Reduces the corners of this bounding box using the specified associative binary operator. */
  def reduce(p: (I, I) => I): I

  /** Builds a new bounding box by apply a function to the corresponding corners of this and another
    * bounding box. */
  def compwise(that: Repr)(p: (I, I) => I): Repr

  /** Builds a new bounding box who's corners are the minimum of the corresponding corners of this
    * and another bounding box. */
  def min(that: Repr): Repr

  /** Builds a new bounding box who's corners are the maximum of the corresponding corners of this
    * and another bounding box. */
  def max(that: Repr): Repr

  /** The area enclosed by the bounding box (number of cells). */
  def area: Elem

  /** Create a bounding box which expands the current bounding box to contain the given cell index. */
  def expand(idx: I): Repr

  /** Create a bounding box which grows the current bounding box by the given amounts in all directions. */
  def grow(delta: I): Repr

  /** Create a bounding box which contains the volume of both this and the given bounding box. */
  def union(that: Repr): Repr

  /** The range (number of cells in each dimension) of the bounding box . */
  def range: I

  /** Clamp the given cell index to the bounds of the box. */
  def clamp(idx: I): I

  /** Split a bounding box into equal halves along the given dimension. */
  def split(dimen: Int): (Repr, Repr)

  /** Split a bounding box along the given cell index in the given dimension
    * such that the lower resulting box contains this split index.
    */
  def split(dimen: Int, idx: Int): (Repr, Repr)

  /** Generate a random cell index inside the bounding box. */
  def randomIndex(): I

  /** Generate a random cell index inside the bounding box.
    *
    * @param gen The random number generator to use.
    */
  def randomIndex(gen: Random): I

  /** Whether the given cell index in inside of the bounding box. */
  def isInside(idx: I): Boolean

  /** The bounding box of the region shared (common cells) with the given bounding box, if any. */
  def intersection(that: Repr): Option[Repr]

  /** Whether the given bounding box shares any cells with this bounding box. */
  def intersects(that: Repr): Boolean

  /** Convert to a 3-dimensional bounding box. */
  def toBCase3i: BCase3i

  /** Convert to a 2-dimensional bounding box. */
  def toBCase2i: BCase2i

  /** Convert to a 1-dimensional bounding box. */
  def toBCase1i: BCase1i

  /** Convert to a more compact String representation. */
  def toPretty: String
}
