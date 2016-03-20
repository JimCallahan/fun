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

/** Base trait for all bounding cases (boxes of indexed cells). */
trait BCase

/** Bounding case component access. */
trait BCaseAccess[P <: Tuple, Repr]
    extends BCase {
  /** The number of dimensions. */
  def dimens: Int

  /** The minimum valued corner index of this bounding case. */
  def bmin: P

  /** The maximum valued corner index of this bounding case. */
  def bmax: P

  /** All corners of the bounding case (unordered). */
  def corners: List[P]

  /** The component identified by the given index. */
  def apply(i: Int): P

  /** A copy of this bounding case in which the minimum corner has been replaced with the given index. */
  def updateMin(p: P): Repr

  /** A copy of this bounding case in which the maximum corner has been replaced with the given index. */
  def updateMax(p: P): Repr

  /** A copy of this bounding case in which the corner identified by index has been replaced with the
    * given value. */
  def update(i: Int, p: P): Repr
}
