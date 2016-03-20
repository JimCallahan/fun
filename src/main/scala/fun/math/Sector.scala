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

/** Base trait for all Sectors (slice of a ring). */
trait Sector
    extends Annulus

/** Sector component access. */
trait SectorAccess[@specialized(Double) Elem, P <: Pos, V <: Vec, Repr]
    extends AnnulusAccess[Elem, P, Repr]
    with Sector {

  /** The left (counter-clockwise) edge vector (normalized). */
  def left: V

  /** The right (clockwise) edge vector (normalized). */
  def right: V

  /** A copy of this Sector in which the left edge has been replaced with the given vector. */
  def updateLeft(v: V): Repr

  /** A copy of this Sector in which the right edge has been replaced with the given vector. */
  def updateRight(v: V): Repr
}
