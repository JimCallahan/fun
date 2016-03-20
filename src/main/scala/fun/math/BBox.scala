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

/** Base trait for all bounding boxes. */
trait BBox

/** Bounding box component access. */
trait BBoxAccess[P <: Pos, Repr]
    extends BBox {
  /** The number of dimensions. */
  def dimens: Int

  /** The minimum valued corner of this bounding box. */
  def bmin: P

  /** The maximum valued corner of this bounding box. */
  def bmax: P

  /** All corners of the bounding box (unordered). */
  def corners: List[P]

  /** The component identified by the given index. */
  def apply(i: Int): P

  /** A copy of this bounding box in which the minimum corner has been replaced with the given position. */
  def updateMin(p: P): Repr

  /** A copy of this bounding box in which the maximum corner has been replaced with the given position. */
  def updateMax(p: P): Repr

  /** A copy of this bounding box in which the corner identified by index has been replaced with the
    * given value. */
  def update(i: Int, p: P): Repr
}
