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

/** Base trait for all spaces. */
trait Space

/** A regular subdivision of a axis aligned rectangular grid of space into cells. */
trait SpaceAccess[P <: Pos, Index <: Tuple, Box <: BBox]
    extends BBoxAccess[P, Box]
    with Space {

  /** The number of cells in each dimension. */
  def size: Index
}
