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

/** An immutable interval of values.
  *
  * @constructor Create a new interval.
  * @param lower The lower bounds.
  * @param isLowerInclusive Whether the lower bounds is inclusive (or exclusive).
  * @param upper The upper bounds.
  * @param isUpperInclusive Whether the upper bounds is inclusive (or exclusive).
  */
class Interval[T](
  val lower: T,
  val isLowerInclusive: Boolean,
  val upper: T,
  val isUpperInclusive: Boolean
)(implicit orderer: T => Ordered[T]) {

  override def equals(that: Any): Boolean =
    that match {
      case that: Interval[T] =>
        (that canEqual this) &&
          (lower == that.lower) && (isLowerInclusive == that.isLowerInclusive) &&
          (upper == that.upper) && (isUpperInclusive == that.isUpperInclusive)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Interval[T] => true
      case _                 => false
    }

  override def hashCode: Int =
    53 * (47 * (43 * (41 + lower.##) + isLowerInclusive.##) + upper.##) + isUpperInclusive.##

  /** Whether the value is below the lower bounds. */
  def isBelow(v: T): Boolean =
    if (isLowerInclusive) v < lower else v <= lower

  /** Whether the value is above the upper bounds. */
  def isAbove(v: T): Boolean =
    if (isUpperInclusive) v > upper else v >= upper

  /** Whether the value is within the interval. */
  def isInside(v: T): Boolean =
    !isBelow(v) && !isAbove(v)

  /** Create an interval with a lower bounds equal to (but not intersecting) the current
    * upper bounds.
    *
    * @param newUpper The new upper bounds.
    * @param upperIncl Whether the new upper bounds is inclusive (or exclusive).
    */
  def shiftUp(newUpper: T, upperIncl: Boolean = false): Interval[T] =
    Interval(upper, !isUpperInclusive, newUpper, upperIncl)

  /** Create an interval with an upper bounds equal to (but not intersecting) the current
    * lower bounds.
    *
    * @param newLower The new lower bounds.
    * @param lowerIncl Whether the new lower bounds is inclusive (or exclusive).
    */
  def shiftDown(newLower: T, lowerIncl: Boolean = true): Interval[T] =
    Interval(newLower, lowerIncl, lower, !isLowerInclusive)

  /** Convert to a string representation. */
  override def toString() =
    ("Interval" +
      (if (isLowerInclusive) "[" else "(") +
      lower + ", " + upper +
      (if (isUpperInclusive) "]" else ")"))
}

object Interval {
  /** Create an interval: [lower, upper)
    *
    * @param lower The inclusive lower bounds.
    * @param upper The exclusive upper bounds.
    */
  def apply[T](lower: T, upper: T)(implicit orderer: T => Ordered[T]): Interval[T] =
    new Interval(lower, true, upper, false)

  /** Create an interval with specific boundary conditions.
    *
    * @param lower The lower bounds.
    * @param lowerIncl Whether the lower bounds is inclusive (or exclusive).
    * @param upper The upper bounds.
    * @param upperIncl Whether the upper bounds is inclusive (or exclusive).
    */
  def apply[T](
    lower: T,
    lowerIncl: Boolean,
    upper: T,
    upperIncl: Boolean
  )(implicit orderer: T => Ordered[T]): Interval[T] =
    new Interval(lower, lowerIncl, upper, upperIncl)
}
