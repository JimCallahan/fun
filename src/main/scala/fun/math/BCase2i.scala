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

class BCase2i protected (val bmin: Index2i, val bmax: Index2i)
    extends BCaseAccess[Index2i, BCase2i]
    with BCaseOps[Int, Index2i, BCase2i] {

  def dimens: Int = 2

  def corners: List[Index2i] = List(bmin, bmin.updateX(bmax.x), bmax, bmax.updateX(bmin.x))

  def apply(i: Int): Index2i =
    i match {
      case 0 => bmin
      case 1 => bmax
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateMin(idx: Index2i): BCase2i = BCase2i(idx, bmax)
  def updateMax(idx: Index2i): BCase2i = BCase2i(bmin, idx)

  def update(i: Int, idx: Index2i): BCase2i =
    i match {
      case 0 => BCase2i(idx, bmax)
      case 1 => BCase2i(bmin, idx)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: BCase2i =>
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass. */
  def canEqual(that: Any): Boolean =
    that match {
      case that: BCase2i => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + bmin.hashCode) + bmax.hashCode

  def forall(p: (Index2i) => Boolean): Boolean = p(bmin) && p(bmax)
  def forall(that: BCase2i)(p: (Index2i, Index2i) => Boolean): Boolean =
    p(bmin, that.bmin) && p(bmax, that.bmax)

  def forany(p: (Index2i) => Boolean): Boolean = p(bmin) || p(bmax)
  def forany(that: BCase2i)(p: (Index2i, Index2i) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  def foreach(p: (Index2i) => Unit): Unit = { p(bmin); p(bmax) }

  def map(p: (Index2i) => Index2i): BCase2i = BCase2i(p(bmin), p(bmax))

  def reduce(p: (Index2i, Index2i) => Index2i): Index2i = p(bmin, bmax)

  def compwise(that: BCase2i)(p: (Index2i, Index2i) => Index2i): BCase2i =
    BCase2i(p(bmin, that.bmin), p(bmax, that.bmax))
  def min(that: BCase2i): BCase2i = compwise(that)(_ min _)
  def max(that: BCase2i): BCase2i = compwise(that)(_ max _)

  def area: Int = (1 /: range)(_ * _)

  def expand(idx: Index2i): BCase2i = BCase2i(idx min bmin, idx max bmax)

  def grow(delta: Index2i): BCase2i = BCase2i(bmin - delta, bmax + delta)

  def union(that: BCase2i): BCase2i = BCase2i(bmin min that.bmin, bmax max that.bmax)

  def range: Index2i = bmax - bmin + 1

  def clamp(idx: Index2i): Index2i = (idx max bmin) min bmax

  def split(dimen: Int): (BCase2i, BCase2i) = {
    val r = range(dimen)
    require(r % 2 == 0, "Cannot split in half along a direction with odd length!")
    val half = r / 2
    (updateMax(bmax.update(dimen, bmin(dimen) + half - 1)),
      updateMin(bmin.update(dimen, bmin(dimen) + half)))
  }
  def splitX: (BCase2i, BCase2i) = split(0)
  def splitY: (BCase2i, BCase2i) = split(1)

  def split(dimen: Int, idx: Int): (BCase2i, BCase2i) = {
    require((idx >= bmin(dimen)) && ((idx + 1) <= bmax(dimen)))
    (updateMax(bmax.update(dimen, idx)), updateMin(bmin.update(dimen, idx + 1)))
  }
  def splitX(idx: Int): (BCase2i, BCase2i) = split(0, idx)
  def splitY(idx: Int): (BCase2i, BCase2i) = split(1, idx)

  def randomIndex(): Index2i =
    bmin + Index2i(Random.nextInt(range.x), Random.nextInt(range.y))
  def randomIndex(gen: Random): Index2i =
    bmin + Index2i(gen.nextInt(range.x), gen.nextInt(range.y))

  def isInside(idx: Index2i): Boolean = idx.forall(bmin)(_ >= _) && idx.forall(bmax)(_ <= _)
  def intersection(that: BCase2i): Option[BCase2i] = {
    val mn = bmin max that.bmin
    val mx = bmax min that.bmax
    if (!mn.forany(mx)(_ >= _)) Some(BCase2i(mn, mx)) else None
  }
  def intersects(that: BCase2i): Boolean = !((bmin max that.bmin).forany(bmax min that.bmax)(_ >= _))

  /** Convert to a String representation */
  override def toString: String = s"BCase2i$toPretty"

  def toPretty: String = s"(${bmin.toPretty}, ${bmax.toPretty})"

  def toBCase3i: BCase3i = BCase3i(bmin.toIndex3i, bmax.toIndex3i)
  def toBCase2i: BCase2i = this
  def toBCase1i: BCase1i = BCase1i(bmin.toIndex1i, bmax.toIndex1i)
}

object BCase2i {
  /** Create a new bounding box given two corner cell indices. */
  def apply(bmin: Index2i, bmax: Index2i): BCase2i = new BCase2i(bmin min bmax, bmin max bmax)

  def unapply(a: BCase2i): Some[(Index2i, Index2i)] = Some((a.bmin, a.bmax))

  /** Create a new bounding box which contains the given cell indices. */
  def apply(indices: Index2i*): BCase2i =
    if (indices.isEmpty) BCase2i(Index2i(0), Index2i(0))
    else indices.toArray.splitAt(1) match {
      case (Array(hd), tl) => new BCase2i((hd /: tl)(_ min _), (hd /: tl)(_ max _))
    }
}
