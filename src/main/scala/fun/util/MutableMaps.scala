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

import scala.collection.immutable
import scala.collection.mutable.Map
import scala.collection.mutable.Set

/** Utility methods for operating on mutable Maps and Sets. */
object MutableMaps {

  /** Construct a map of sets from tuples. */
  def mapSet[A, B](data: Iterable[(A, B)]): Map[A, Set[B]] = {
    val m: Map[A, Set[B]] = Map.empty
    for ((a, b) <- data)
      m.getOrElseUpdate(a, Set.empty) += b
    m
  }

  /** Deconstruct a map of sets into tuples. */
  def unMapSet[A, B](m: Map[A, Set[B]]): Iterable[(A, B)] =
    for {
      (a, s) <- m.toSeq
      b <- s
    } yield (a, b)

  /** Construct a double level map from tuples. */
  def mapMap[A, B, C](data: Iterable[(A, B, C)]): Map[A, Map[B, C]] = {
    val m: Map[A, Map[B, C]] = Map.empty
    for ((a, b, c) <- data)
      m.getOrElseUpdate(a, Map.empty) += (b -> c)
    m
  }

  /** Deconstruct a double level map into tuples. */
  def unMapMap[A, B, C](m: Map[A, Map[B, C]]): Iterable[(A, B, C)] =
    for {
      (a, mm) <- m.toSeq
      (b, c) <- mm
    } yield (a, b, c)

  /** Construct a double level map of sets from tuples. */
  def mapMapSet[A, B, C](data: Iterable[(A, B, C)]): Map[A, Map[B, Set[C]]] = {
    val m: Map[A, Map[B, Set[C]]] = Map.empty
    for ((a, b, c) <- data)
      m.getOrElseUpdate(a, Map.empty).getOrElseUpdate(b, Set.empty) += c
    m
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
    val m: Map[A, Map[B, Map[C, D]]] = Map.empty
    for ((a, b, c, d) <- data)
      m.getOrElseUpdate(a, Map.empty).getOrElseUpdate(b, Map.empty) += (c -> d)
    m
  }

  /** Deconstruct a double level map of sets into tuples. */
  def unMapMapMap[A, B, C, D](m: Map[A, Map[B, Map[C, D]]]): Iterable[(A, B, C, D)] =
    for {
      (a, mm) <- m.toSeq
      (b, mmm) <- mm
      (c, d) <- mmm
    } yield (a, b, c, d)

  /** Convert into a immutable map. */
  def freezeMap[A, B](m: Map[A, B]): immutable.Map[A, B] =
    immutable.Map.empty ++ m

  /** Convert into a immutable map of sets. */
  def freezeMapSet[A, B](m: Map[A, Set[B]]): immutable.Map[A, immutable.Set[B]] =
    immutable.Map.empty ++ m.mapValues(immutable.Set.empty ++ _)

  /** Convert into a immutable double level map. */
  def freezeMapMap[A, B, C](m: Map[A, Map[B, C]]): immutable.Map[A, immutable.Map[B, C]] =
    immutable.Map.empty ++ m.mapValues(freezeMap _)

  /** Convert into a immutable double level map of sets. */
  def freezeMapMapSet[A, B, C](m: Map[A, Map[B, Set[C]]]): immutable.Map[A, immutable.Map[B, immutable.Set[C]]] =
    immutable.Map.empty ++ m.mapValues(freezeMapSet _)

  /** Convert into a immutable triple level map. */
  def freezeMapMapMap[A, B, C, D](m: Map[A, Map[B, Map[C, D]]]): immutable.Map[A, immutable.Map[B, immutable.Map[C, D]]] =
    immutable.Map.empty ++ m.mapValues(freezeMapMap _)

  /** Construct a map from tuples in which the second value is aggregated using the given operation. */
  def aggMap[A, B](data: Iterable[(A, B)])(op: (B, B) => B): Map[A, B] = {
    val m: Map[A, B] = Map.empty
    for ((a, b) <- data) {
      val v = m.get(a) match {
        case Some(bb) => op(bb, b)
        case _        => b
      }
      m += a -> v
    }
    m
  }

  /** Construct a double level map from tuples in which the third value is aggregated using the given operation. */
  def aggMapMap[A, B, C](data: Iterable[(A, B, C)])(op: (C, C) => C): Map[A, Map[B, C]] = {
    val m: Map[A, Map[B, C]] = Map.empty
    for ((a, b, c) <- data) {
      val mm = m.getOrElseUpdate(a, Map.empty)
      val v = mm.get(b) match {
        case Some(cc) => op(cc, c)
        case _        => c
      }
      mm += b -> v
    }
    m
  }

  /** Construct a triple level map from tuples in which the fourth value is aggregated using the given operation. */
  def aggMapMapMap[A, B, C, D](data: Iterable[(A, B, C, D)])(op: (D, D) => D): Map[A, Map[B, Map[C, D]]] = {
    val m: Map[A, Map[B, Map[C, D]]] = Map.empty
    for ((a, b, c, d) <- data) {
      val mm = m.getOrElseUpdate(a, Map.empty).getOrElseUpdate(b, Map.empty)
      val v = mm.get(c) match {
        case Some(dd) => op(dd, d)
        case _        => d
      }
      mm += c -> v
    }
    m
  }
}
