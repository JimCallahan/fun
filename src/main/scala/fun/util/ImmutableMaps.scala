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
package fun.util

import scala.collection.mutable
import scala.collection.immutable.Map
import scala.collection.immutable.Set

/** Utility methods for operating on immutable Maps and Sets. */
object ImmutableMaps {

  /** Construct a map of sets from tuples. */
  def mapSet[A, B](data: Iterable[(A, B)]): Map[A, Set[B]] = {
    val empty: Map[A, Set[B]] = Map.empty
      (empty /: data) {
      case (m, (a, b)) => m + (a -> (m.getOrElse(a, Set.empty) + b))
    }
  }

  /** Deconstruct a map of sets into tuples. */
  def unMapSet[A, B](m: Map[A, Set[B]]): Iterable[(A, B)] =
    for {
      (a, s) <- m.toSeq
      b <- s
    } yield (a, b)

  /** Construct a double level map from tuples. */
  def mapMap[A, B, C](data: Iterable[(A, B, C)]): Map[A, Map[B, C]] = {
    val empty: Map[A, Map[B, C]] = Map.empty
      (empty /: data) {
      case (m, (a, b, c)) => m + (a -> (m.getOrElse(a, Map.empty) + (b -> c)))
    }
  }

  /** Deconstruct a double level map into tuples. */
  def unMapMap[A, B, C](m: Map[A, Map[B, C]]): Iterable[(A, B, C)] =
    for {
      (a, mm) <- m.toSeq
      (b, c) <- mm
    } yield (a, b, c)

  /** Construct a double level map of sets from tuples. */
  def mapMapSet[A, B, C](data: Iterable[(A, B, C)]): Map[A, Map[B, Set[C]]] = {
    val empty: Map[A, Map[B, Set[C]]] = Map.empty
      (empty /: data) {
      case (m, (a, b, c)) =>
        val mm = m.getOrElse(a, Map.empty)
        m + (a -> (mm + (b -> (mm.getOrElse(b, Set.empty) + c))))
    }
  }

  /** Deconstruct a double level map of sets into tuples. */
  def unMapMapSet[A, B, C](m: Map[A, Map[B, Set[C]]]): Iterable[(A, B, C)] =
    for {
      (a, mm) <- m.toSeq
      (b, s) <- mm
      c <- s
    } yield (a, b, c)

  /** Construct a triple level map from tuples. */
  def mapMapMap[A, B, C, D](data: Iterable[(A, B, C, D)]): Map[A, Map[B, Map[C, D]]] = {
    val empty: Map[A, Map[B, Map[C, D]]] = Map.empty
      (empty /: data) {
      case (m, (a, b, c, d)) =>
        val mm = m.getOrElse(a, Map.empty)
        m + (a -> (mm + (b -> (mm.getOrElse(b, Map.empty) + (c -> d)))))
    }
  }

  /** Deconstruct a double level map of sets into tuples. */
  def unMapMapMap[A, B, C, D](m: Map[A, Map[B, Map[C, D]]]): Iterable[(A, B, C, D)] =
    for {
      (a, mm) <- m.toSeq
      (b, mmm) <- mm
      (c, d) <- mmm
    } yield (a, b, c, d)

  /** Convert into a mutable map. */
  def thawMap[A, B](m: Map[A, B]): mutable.Map[A, B] =
    mutable.Map.empty ++ m

  /** Convert into a mutable map of sets. */
  def thawMapSet[A, B](m: Map[A, Set[B]]): mutable.Map[A, mutable.Set[B]] =
    mutable.Map.empty ++ m.mapValues(mutable.Set.empty ++ _)

  /** Convert into a mutable double level map. */
  def thawMapMap[A, B, C](m: Map[A, Map[B, C]]): mutable.Map[A, mutable.Map[B, C]] =
    mutable.Map.empty ++ m.mapValues(thawMap _)

  /** Convert into a mutable double level map of sets. */
  def thawMapMapSet[A, B, C](m: Map[A, Map[B, Set[C]]]): mutable.Map[A, mutable.Map[B, mutable.Set[C]]] =
    mutable.Map.empty ++ m.mapValues(thawMapSet _)

  /** Convert into a mutable triple level map. */
  def thawMapMapMap[A, B, C, D](m: Map[A, Map[B, Map[C, D]]]): mutable.Map[A, mutable.Map[B, mutable.Map[C, D]]] =
    mutable.Map.empty ++ m.mapValues(thawMapMap _)
}
