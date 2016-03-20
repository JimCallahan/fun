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

/** A regular subdivision of a axis aligned rectangular grid of space into cells. */
trait SpaceOps[P <: Pos, V <: Vec, Index <: Tuple, Box <: BBox, Repr <: Space]
    extends Numeric {

  /** Whether another space is within a given epsilon of this space. */
  def equiv(that: Repr, epsilon: Double): Boolean

  /** Whether another space is within a the standard epsilon of this space. */
  def equiv(that: Repr): Boolean

  /** The size of a individual cell. */
  def cellSize: V

  /** The world space bounding box of the cell with the given index. */
  def cellBounds(idx: Index): Box

  /** The local coordinate bounding box of the cell with the given index. */
  def cellCoords(idx: Index): Box

  /** The index of the voxel cell containing the given world space position. */
  def indexOf(pos: P): Index

  /** The index of the voxel cell containing the given world space position and the index offsets
    * to the nearest neighboring cells.
    *
    * The index offset is an integer vector containing either 1 or -1 for each dimension indicating
    * the direction in index space of the nearest neighboring cell.
    * @return The index and index offset as a tuple.
    */
  def offsetsOf(pos: P): (Index, Index)

  /** The index of the voxel cell containing the given world space position, index offsets to the
    * nearest neighboring cells and interpolation vector with these cells.
    *
    * The index offset is an integer vector containing either 1 or -1 for each dimension indicating
    * the direction in index space of the nearest neighboring cell.
    *
    * The interpolation vector is a floating point vector with values in the range [0.0, 1.0) which
    * provides the weighting to use when interpolating values of neighboring cells indiced by the
    * index and index offsets.
    * @return The index, index offset and interpolation vector as a tuple.
    */
  def interpOf(pos: P): (Index, Index, V)

  /** Convert to a bounding box. */
  def toBBox: Box
}
