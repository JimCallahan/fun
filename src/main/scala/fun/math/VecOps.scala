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

trait VecOps[@specialized(Double, Int) Elem, Repr <: Tuple]
    extends TupleOps[Elem, Repr, Repr] {
  /** The dot-product of this and another vector. */
  def dot(that: Repr): Elem

  /** The magnitude of this vector squared. */
  def magSq: Elem

  /** The magnitude of this vector. */
  def mag: Elem

  /** A vector of identical direction but unit length. */
  def normalized: Repr
}