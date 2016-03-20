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

import scala.util.Random

class BCase1i protected (val bmin: Index1i, val bmax: Index1i)
    extends BCaseAccess[Index1i, BCase1i]
    with BCaseOps[Int, Index1i, BCase1i] {

  def dimens: Int = 1

  def corners: List[Index1i] = List(bmin, bmax)

  def apply(i: Int): Index1i =
    i match {
      case 0 => bmin
      case 1 => bmax
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateMin(idx: Index1i): BCase1i = BCase1i(idx, bmax)
  def updateMax(idx: Index1i): BCase1i = BCase1i(bmin, idx)

  def update(i: Int, idx: Index1i): BCase1i =
    i match {
      case 0 => BCase1i(idx, bmax)
      case 1 => BCase1i(bmin, idx)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: BCase1i =>
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean =
    that match {
      case that: BCase1i => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + bmin.hashCode) + bmax.hashCode

  def forall(p: (Index1i) => Boolean): Boolean = p(bmin) && p(bmax)
  def forall(that: BCase1i)(p: (Index1i, Index1i) => Boolean): Boolean =
    p(bmin, that.bmin) && p(bmax, that.bmax)

  def forany(p: (Index1i) => Boolean): Boolean = p(bmin) || p(bmax)
  def forany(that: BCase1i)(p: (Index1i, Index1i) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  def foreach(p: (Index1i) => Unit): Unit = { p(bmin); p(bmax) }

  def map(p: (Index1i) => Index1i): BCase1i = BCase1i(p(bmin), p(bmax))

  def reduce(p: (Index1i, Index1i) => Index1i): Index1i = p(bmin, bmax)

  def compwise(that: BCase1i)(p: (Index1i, Index1i) => Index1i): BCase1i =
    BCase1i(p(bmin, that.bmin), p(bmax, that.bmax))
  def min(that: BCase1i): BCase1i = compwise(that)(_ min _)
  def max(that: BCase1i): BCase1i = compwise(that)(_ max _)

  def area: Int = (1 /: range)(_ * _)

  def expand(idx: Index1i): BCase1i = BCase1i(idx min bmin, idx max bmax)

  def grow(delta: Index1i): BCase1i = BCase1i(bmin - delta, bmax + delta)

  def union(that: BCase1i): BCase1i = BCase1i(bmin min that.bmin, bmax max that.bmax)

  def range: Index1i = bmax - bmin + 1

  def clamp(idx: Index1i): Index1i = (idx max bmin) min bmax

  def split(dimen: Int): (BCase1i, BCase1i) = {
    val r = range(dimen)
    require(r % 2 == 0, "Cannot split in half along a direction with odd length!")
    val half = r / 2
    (updateMax(bmax.update(dimen, bmin(dimen) + half - 1)),
      updateMin(bmin.update(dimen, bmin(dimen) + half)))
  }
  def splitX: (BCase1i, BCase1i) = split(0)

  def split(dimen: Int, idx: Int): (BCase1i, BCase1i) = {
    require((idx >= bmin(dimen)) && ((idx + 1) <= bmax(dimen)))
    (updateMax(bmax.update(dimen, idx)), updateMin(bmin.update(dimen, idx + 1)))
  }
  def splitX(idx: Int): (BCase1i, BCase1i) = split(0, idx)

  def randomIndex(): Index1i =
    bmin + Index1i(Random.nextInt(range.x))
  def randomIndex(gen: Random): Index1i =
    bmin + Index1i(gen.nextInt(range.x))

  def isInside(idx: Index1i): Boolean = idx.forall(bmin)(_ >= _) && idx.forall(bmax)(_ <= _)
  def intersection(that: BCase1i): Option[BCase1i] = {
    val mn = bmin max that.bmin
    val mx = bmax min that.bmax
    if (!mn.forany(mx)(_ >= _)) Some(BCase1i(mn, mx)) else None
  }
  def intersects(that: BCase1i): Boolean =
    !((bmin max that.bmin).forany(bmax min that.bmax)(_ >= _))

  /** Convert to a String representation */
  override def toString: String = s"BCase1i$toPretty"

  def toPretty: String = s"(${bmin.toPretty}, ${bmax.toPretty})"

  def toBCase3i: BCase3i = BCase3i(bmin.toIndex3i, bmax.toIndex3i)
  def toBCase2i: BCase2i = BCase2i(bmin.toIndex2i, bmax.toIndex2i)
  def toBCase1i: BCase1i = this

  /** Convert to an Interval. */
  def toInterval: Interval[Int] = Interval(bmin.x, bmax.x)
}

object BCase1i {
  /** Create a new bounding box given two corner cell indices. */
  def apply(bmin: Index1i, bmax: Index1i): BCase1i = new BCase1i(bmin min bmax, bmin max bmax)

  def unapply(a: BCase1i): Some[(Index1i, Index1i)] = Some((a.bmin, a.bmax))

  /** Create a new bounding box which contains the given cell indices. */
  def apply(indices: Index1i*): BCase1i =
    if (indices.isEmpty) BCase1i(Index1i(0), Index1i(0))
    else indices.toArray.splitAt(1) match {
      case (Array(hd), tl) => new BCase1i((hd /: tl)(_ min _), (hd /: tl)(_ max _))
    }
}
