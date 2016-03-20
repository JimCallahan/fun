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

/** A collection of convenience methods for scalar values. */
object Scalar {
  import scala.math.{ max, ulp }

  /** Whether the two values are within a given epsilon of each other. */
  def equiv(a: Short, b: Short, epsilon: Short): Boolean =
    if (a < b) a + epsilon >= b
    else b + epsilon >= a

  /** Whether the two values are within a given epsilon of each other. */
  def equiv(a: Int, b: Int, epsilon: Int): Boolean =
    if (a < b) a + epsilon >= b
    else b + epsilon >= a

  /** Whether the two values are within a given epsilon of each other. */
  def equiv(a: Long, b: Long, epsilon: Long): Boolean =
    if (a < b) a + epsilon >= b
    else b + epsilon >= a

  /** Whether the two values are within a given epsilon of each other. */
  def equiv(a: Float, b: Float, epsilon: Float): Boolean =
    if (a < b) a + epsilon >= b
    else b + epsilon >= a

  /** Whether the two values are within a given epsilon of each other. */
  def equiv(a: Double, b: Double, epsilon: Double): Boolean =
    if (a < b) a + epsilon >= b
    else b + epsilon >= a

  /** Whether the two values are within a type specific minimal epsilon. */
  def equiv(a: Short, b: Short): Boolean =
    a == b

  /** Whether the two values are within a type specific minimal epsilon. */
  def equiv(a: Int, b: Int): Boolean =
    a == b

  /** Whether the two values are within a type specific minimal epsilon. */
  def equiv(a: Long, b: Long): Boolean =
    a == b

  /** Whether the two values are within a type specific minimal epsilon. */
  def equiv(a: Float, b: Float): Boolean =
    equiv(a, b, max(ulp(a), ulp(b)) * 1.0E3f)

  /** Whether the two values are within a type specific minimal epsilon. */
  def equiv(a: Double, b: Double): Boolean =
    equiv(a, b, max(ulp(a), ulp(b)) * 1.0E6)

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Short, lower: Short, upper: Short): Short =
    if (v < lower) lower else if (v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Int, lower: Int, upper: Int): Int =
    if (v < lower) lower else if (v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Long, lower: Long, upper: Long): Long =
    if (v < lower) lower else if (v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Float, lower: Float, upper: Float): Float =
    if (v < lower) lower else if (v > upper) upper else v

  /** Clamp a value to be between the given upper and lower bounds. */
  def clamp(v: Double, lower: Double, upper: Double): Double =
    if (v < lower) lower else if (v > upper) upper else v

  /** Linearly interpolate between two values. */
  def lerp(a: Float, b: Float, t: Float): Float =
    a + t * (b - a)

  /** Linearly interpolate between two values. */
  def lerp(a: Double, b: Double, t: Double): Double =
    a + t * (b - a)

  /** Smooth-step interpolate between two values. */
  def smoothlerp(a: Float, b: Float, t: Float): Float =
    lerp(a, b, smoothstep(t))

  /** Smooth-step interpolate between two values. */
  def smoothlerp(a: Double, b: Double, t: Double): Double =
    lerp(a, b, smoothstep(t))

  /** The smooth-step interpolation function. */
  def smoothstep(t: Float): Float =
    (3.0f * t * t) - (2.0f * t * t * t)

  /** The smooth-step interpolation function. */
  def smoothstep(t: Double): Double =
    (3.0 * t * t) - (2.0 * t * t * t)

  import scala.language.implicitConversions

  /** Implicit conversions from a Double scalar to 1-dimensional position. */
  implicit def doubleToPos1d(d: Double): Pos1d = Pos1d(d)

  /** Implicit conversions from a 1-dimensional position to Double scalar. */
  implicit def pos1dToDouble(p: Pos1d): Double = p.x

  /** Implicit conversions from a Double scalar to 1-dimensional vector. */
  implicit def doubleToVec1d(d: Double): Vec1d = Vec1d(d)

  /** Implicit conversions from a 1-dimensional vector to Double scalar. */
  implicit def vec1dToDouble(v: Vec1d): Double = v.x

  /** Implicit conversions from a Int scalar to 1-dimensional index. */
  implicit def intToIndex1i(i: Int): Index1i = Index1i(i)

  /** Implicit conversions from a 1-dimensional index to Int scalar. */
  implicit def index1iToInt(i: Index1i): Int = i.x
}
