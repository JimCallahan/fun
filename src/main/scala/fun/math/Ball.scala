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

/** Base trait for all balls (space inside a circle or sphere). */
trait Ball

/** Ball component access. */
trait BallAccess[@specialized(Double) Elem, P <: Pos, Repr]
    extends Ball {

  /** The number of dimensions. */
  def dimens: Int

  /** The center of this Ball. */
  def center: P

  /** The radius of this Ball. */
  def radius: Elem

  /** A copy of this Ball in which center has been replaced with the given position. */
  def updateCenter(p: P): Repr

  /** A copy of this Ball in which the radius has been replaced with the given radius. */
  def updateRadius(r: Elem): Repr
}
