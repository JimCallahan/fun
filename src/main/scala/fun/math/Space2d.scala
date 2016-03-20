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

class Space2d private (bn: Pos2d, bx: Pos2d, val size: Index2i)
    extends BBox2d(bn, bx)
    with SpaceAccess[Pos2d, Index2i, BBox2d]
    with SpaceOps[Pos2d, Vec2d, Index2i, BBox2d, Space2d] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Space2d =>
        (that canEqual this) &&
          (bmin == that.bmin) && (bmax == that.bmax) && (size == that.size)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  override def canEqual(that: Any): Boolean =
    that match {
      case that: Space2d => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    53 * (47 * (43 + bmin.hashCode) + bmax.hashCode) + size.hashCode

  def equiv(that: Space2d, epsilon: Double): Boolean =
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon) && (size == that.size)

  def equiv(that: Space2d): Boolean =
    (bmin equiv that.bmin) && (bmax equiv that.bmax) && (size == that.size)

  def cellSize: Vec2d = range / size.toVec2d

  def cellBounds(idx: Index2i): BBox2d = cellCoords(idx).map(position)

  def cellCoords(idx: Index2i): BBox2d =
    BBox2d(idx.toPos2d / size.toVec2d, (idx + 1).toPos2d / size.toVec2d)

  def indexOf(pos: Pos2d): Index2i =
    (coord(pos) * size.toVec2d).map(scala.math.floor _).toIndex2i

  def offsetsOf(pos: Pos2d): (Index2i, Index2i) = {
    val c = coord(pos) * size.toVec2d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec2d(0.5)
    (w.toIndex2i, o.map(scala.math.signum _).toIndex2i)
  }

  def interpOf(pos: Pos2d): (Index2i, Index2i, Vec2d) = {
    val c = coord(pos) * size.toVec2d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec2d(0.5)
    (w.toIndex2i, o.map(scala.math.signum _).toIndex2i, o.map(scala.math.abs))
  }

  /** Convert to a string representation. */
  override def toString() =
    "Space2d(" + bmin + ", " + bmax + ", " + size + ")"

  def toBBox: BBox2d = BBox2d(bmin, bmax)
}

object Space2d {
  /** Create a new space who's bounds are from [0, 1] in all dimensions.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index2i): Space2d = {
    testSize(size)
    new Space2d(Pos2d(0.0), Pos2d(1.0), size)
  }

  /** Create a new space given two corners.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index2i, bbox: BBox2d): Space2d = {
    val BBox2d(bn, bx) = bbox
    testBBox(bbox)
    testSize(size)
    new Space2d(bn, bx, size)
  }

  /** Create a new space given two corners.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index2i, bmin: Pos2d, bmax: Pos2d): Space2d = {
    val (bn, bx) = (bmin min bmax, bmin max bmax)
    testBBox(BBox2d(bn, bx))
    testSize(size)
    new Space2d(bn, bx, size)
  }

  /** Create a new bounding box which contains the given points. */
  def apply(size: Index2i, pts: Pos2d*): Space2d = {
    if (pts.isEmpty) throw new IllegalArgumentException("Spaces must have a positive volume!")
    else {
      val bbox @ BBox2d(bn, bx) = BBox2d(pts: _*)
      testBBox(bbox)
      testSize(size)
      new Space2d(bn, bx, size)
    }
  }

  def unapply(s: Space2d): Some[(Index2i, Pos2d, Pos2d)] = Some((s.size, s.bmin, s.bmax))

  private def testSize(size: Index2i) {
    require(size.forall(_ > 0),
      "The number of voxel cells in each dimension (size) must be a positive number!")
  }

  private def testBBox(bbox: BBox2d) {
    require(bbox.area > 0.0,
      "Spaces must have a positive volume!")
  }
}
