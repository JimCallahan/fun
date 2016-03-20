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

class Space1d private (bn: Pos1d, bx: Pos1d, val size: Index1i)
    extends BBox1d(bn, bx)
    with SpaceAccess[Pos1d, Index1i, BBox1d]
    with SpaceOps[Pos1d, Vec1d, Index1i, BBox1d, Space1d] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Space1d =>
        (that canEqual this) &&
          (bmin == that.bmin) && (bmax == that.bmax) && (size == that.size)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  override def canEqual(that: Any): Boolean =
    that match {
      case that: Space1d => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    53 * (47 * (43 + bmin.hashCode) + bmax.hashCode) + size.hashCode

  def equiv(that: Space1d, epsilon: Double): Boolean =
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon) && (size == that.size)

  def equiv(that: Space1d): Boolean =
    (bmin equiv that.bmin) && (bmax equiv that.bmax) && (size == that.size)

  def cellSize: Vec1d = range / size.toVec1d

  def cellBounds(idx: Index1i): BBox1d = cellCoords(idx).map(position)

  def cellCoords(idx: Index1i): BBox1d =
    BBox1d(idx.toPos1d / size.toVec1d, (idx + 1).toPos1d / size.toVec1d)

  def indexOf(pos: Pos1d): Index1i =
    (coord(pos) * size.toVec1d).map(scala.math.floor _).toIndex1i

  def offsetsOf(pos: Pos1d): (Index1i, Index1i) = {
    val c = coord(pos) * size.toVec1d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec1d(0.5)
    (w.toIndex1i, o.map(scala.math.signum _).toIndex1i)
  }

  def interpOf(pos: Pos1d): (Index1i, Index1i, Vec1d) = {
    val c = coord(pos) * size.toVec1d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec1d(0.5)
    (w.toIndex1i, o.map(scala.math.signum _).toIndex1i, o.map(scala.math.abs))
  }

  /** Convert to a string representation. */
  override def toString() =
    "Space1d(" + bmin + ", " + bmax + ", " + size + ")"

  def toBBox: BBox1d = BBox1d(bmin, bmax)
}

object Space1d {
  /** Create a new space who's bounds are from [0, 1] in all dimensions.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index1i): Space1d = {
    testSize(size)
    new Space1d(Pos1d(0.0), Pos1d(1.0), size)
  }

  /** Create a new space given two corners.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index1i, bbox: BBox1d): Space1d = {
    val BBox1d(bn, bx) = bbox
    testBBox(bbox)
    testSize(size)
    new Space1d(bn, bx, size)
  }

  /** Create a new space given two corners.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index1i, bmin: Pos1d, bmax: Pos1d): Space1d = {
    val (bn, bx) = (bmin min bmax, bmin max bmax)
    testBBox(BBox1d(bn, bx))
    testSize(size)
    new Space1d(bn, bx, size)
  }

  /** Create a new bounding box which contains the given points. */
  def apply(size: Index1i, pts: Pos1d*): Space1d = {
    if (pts.isEmpty) throw new IllegalArgumentException("Spaces must have a positive volume!")
    else {
      val bbox @ BBox1d(bn, bx) = BBox1d(pts: _*)
      testBBox(bbox)
      testSize(size)
      new Space1d(bn, bx, size)
    }
  }

  def unapply(s: Space1d): Some[(Index1i, Pos1d, Pos1d)] = Some((s.size, s.bmin, s.bmax))

  private def testSize(size: Index1i): Unit = {
    require(size.forall(_ > 0),
      "The number of voxel cells in each dimension (size) must be a positive number!")
  }

  private def testBBox(bbox: BBox1d): Unit = {
    require(bbox.area > 0.0,
      "Spaces must have a positive volume!")
  }
}
