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

/** Base trait for all coordinate frames. */
trait Frame

/** Frame component access. */
trait FrameAccess[@specialized(Double) Elem]
    extends Frame {
  /** Lookup a the value of a given cell (column, row) of this matrix. */
  def apply(col: Int, row: Int): Elem

  /** The number of dimensions. */
  val dimens: Int
}

/** A 2-dimensional coordinate frame. */
trait Frame2[@specialized(Double) Elem, V <: Vec, P <: Pos]
    extends FrameAccess[Elem] {
  /** The X-basis vector. */
  val basisX: V

  /** The Y-basis vector. */
  val basisY: V

  /** The origin position. */
  val origin: P

  val dimens = 2
}

/** A 3-dimensional coordinate frame. */
trait Frame3[@specialized(Double) Elem, V <: Vec, P <: Pos]
    extends FrameAccess[Elem] {
  /** The X-basis vector. */
  val basisX: V

  /** The Y-basis vector. */
  val basisY: V

  /** The Z-basis vector. */
  val basisZ: V

  /** The origin position. */
  val origin: P

  val dimens = 3
}
