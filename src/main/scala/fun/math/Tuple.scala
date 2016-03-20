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

import scala.annotation.tailrec

/** Base trait for all tuples. */
trait Tuple

/** A Tuple that represents a position in space. */
trait Pos
    extends Tuple

/** A Tuple that represents an arbitrary vector. */
trait Vec
    extends Tuple

/** Tuple component access. */
trait TupleAccess[@specialized(Double, Int) Elem, Repr]
    extends Tuple {
  /** The number of components. */
  def size: Int

  /** The component identified by the given index. */
  def apply(i: Int): Elem

  /** A copy of this vector in which the component identified by index has been replaced with the
    * given value. */
  def update(i: Int, e: Elem): Repr

  /** Evaluate the operation only if the size of the supplied vector is the same as this one. */
  protected def validate[A](that: TupleAccess[Elem, Repr])(op: => A): A =
    if (size != that.size) throw new IllegalArgumentException("Unequal sized vectors!")
    else op

  /** Perform component-wise equality test on two arrays. */
  protected def arrayEquals[A](x: Array[A], y: Array[A]): Boolean = {
    if (x.size != y.size) false
    else {
      @tailrec def f(i: Int): Boolean = if (i < x.size) (x(i) equals y(i)) && f(i + 1) else true
      f(0)
    }
  }
}

/** A 1-dimensional tuple. */
trait Tuple1[@specialized(Double, Int) Elem, Repr]
    extends TupleAccess[Elem, Repr] {

  def size: Int = 1

  /** The X-component of this vector. */
  val x: Elem

  /** A copy of this vector in which the X-component has been replaced with the given value. */
  def updateX(e: Elem): Repr
}

/** A 2-dimensional tuple. */
trait Tuple2[@specialized(Double, Int) Elem, Repr]
    extends TupleAccess[Elem, Repr] {

  def size: Int = 2

  /** The first component of this vector. */
  val x: Elem

  /** The second component of this vector. */
  val y: Elem

  /** A copy of this vector in which the first component has been replaced with the given value. */
  def updateX(e: Elem): Repr

  /** A copy of this vector in which the second component has been replaced with the given value. */
  def updateY(e: Elem): Repr
}

/** A 3-dimensional tuple. */
trait Tuple3[@specialized(Double, Int) Elem, Repr]
    extends TupleAccess[Elem, Repr] {

  def size: Int = 3

  /** The X-component of this vector. */
  val x: Elem

  /** The Y-component of this vector. */
  val y: Elem

  /** The Z-component of this vector. */
  val z: Elem

  /** A copy of this vector in which the X-component has been replaced with the given value. */
  def updateX(e: Elem): Repr

  /** A copy of this vector in which the Y-component has been replaced with the given value. */
  def updateY(e: Elem): Repr

  /** A copy of this vector in which the Z-component has been replaced with the given value. */
  def updateZ(e: Elem): Repr
}

/** A 4-dimensional tuple. */
trait Tuple4[@specialized(Double, Int) Elem, Repr]
    extends TupleAccess[Elem, Repr] {

  def size: Int = 4

  /** The X-component of this vector. */
  val x: Elem

  /** The Y-component of this vector. */
  val y: Elem

  /** The Z-component of this vector. */
  val z: Elem

  /** The W-component of this vector. */
  val w: Elem

  /** A copy of this vector in which the X-component has been replaced with the given value. */
  def updateX(e: Elem): Repr

  /** A copy of this vector in which the Y-component has been replaced with the given value. */
  def updateY(e: Elem): Repr

  /** A copy of this vector in which the Z-component has been replaced with the given value. */
  def updateZ(e: Elem): Repr

  /** A copy of this vector in which the W-component has been replaced with the given value. */
  def updateW(e: Elem): Repr
}
