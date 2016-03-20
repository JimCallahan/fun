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

class Space3d private (bn: Pos3d, bx: Pos3d, val size: Index3i)
    extends BBox3d(bn, bx)
    with SpaceAccess[Pos3d, Index3i, BBox3d]
    with SpaceOps[Pos3d, Vec3d, Index3i, BBox3d, Space3d] {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Space3d =>
        (that canEqual this) &&
          (bmin == that.bmin) && (bmax == that.bmax) && (size == that.size)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  override def canEqual(that: Any): Boolean =
    that match {
      case that: Space3d => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    53 * (47 * (43 + bmin.hashCode) + bmax.hashCode) + size.hashCode

  def equiv(that: Space3d, epsilon: Double): Boolean =
    bmin.equiv(that.bmin, epsilon) && bmax.equiv(that.bmax, epsilon) && (size == that.size)

  def equiv(that: Space3d): Boolean =
    (bmin equiv that.bmin) && (bmax equiv that.bmax) && (size == that.size)

  def cellSize: Vec3d = range / size.toVec3d

  def cellBounds(idx: Index3i): BBox3d = cellCoords(idx).map(position)

  def cellCoords(idx: Index3i): BBox3d =
    BBox3d(idx.toPos3d / size.toVec3d, (idx + 1).toPos3d / size.toVec3d)

  def indexOf(pos: Pos3d): Index3i =
    (coord(pos) * size.toVec3d).map(scala.math.floor _).toIndex3i

  def offsetsOf(pos: Pos3d): (Index3i, Index3i) = {
    val c = coord(pos) * size.toVec3d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec3d(0.5)
    (w.toIndex3i, o.map(scala.math.signum _).toIndex3i)
  }

  def interpOf(pos: Pos3d): (Index3i, Index3i, Vec3d) = {
    val c = coord(pos) * size.toVec3d
    val w = c.map(scala.math.floor _)
    val o = c - w - Vec3d(0.5)
    (w.toIndex3i, o.map(scala.math.signum _).toIndex3i, o.map(scala.math.abs))
  }

  /** Convert to a string representation. */
  override def toString() =
    "Space3d(" + bmin + ", " + bmax + ", " + size + ")"

  def toBBox: BBox3d = BBox3d(bmin, bmax)
}

object Space3d {
  /** Create a new space who's bounds are from [0, 1] in all dimensions.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index3i): Space3d = {
    testSize(size)
    new Space3d(Pos3d(0.0), Pos3d(1.0), size)
  }

  /** Create a new space given two corners.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index3i, bbox: BBox3d): Space3d = {
    val BBox3d(bn, bx) = bbox
    testBBox(bbox)
    testSize(size)
    new Space3d(bn, bx, size)
  }

  /** Create a new space given two corners.
    * @param size The number of cells in each dimension.
    */
  def apply(size: Index3i, bmin: Pos3d, bmax: Pos3d): Space3d = {
    val (bn, bx) = (bmin min bmax, bmin max bmax)
    testBBox(BBox3d(bn, bx))
    testSize(size)
    new Space3d(bn, bx, size)
  }

  /** Create a new bounding box which contains the given points. */
  def apply(size: Index3i, pts: Pos3d*): Space3d = {
    if (pts.isEmpty) throw new IllegalArgumentException("Spaces must have a positive volume!")
    else {
      val bbox @ BBox3d(bn, bx) = BBox3d(pts: _*)
      testBBox(bbox)
      testSize(size)
      new Space3d(bn, bx, size)
    }
  }

  def unapply(s: Space3d): Some[(Index3i, Pos3d, Pos3d)] = Some((s.size, s.bmin, s.bmax))

  private def testSize(size: Index3i): Unit = {
    require(size.forall(_ > 0),
      "The number of voxel cells in each dimension (size) must be a positive number!")
  }

  private def testBBox(bbox: BBox3d): Unit = {
    require(bbox.area > 0.0,
      "Spaces must have a positive volume!")
  }
}
