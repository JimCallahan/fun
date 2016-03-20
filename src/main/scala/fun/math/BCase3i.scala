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

class BCase3i protected (val bmin: Index3i, val bmax: Index3i)
    extends BCaseAccess[Index3i, BCase3i]
    with BCaseOps[Int, Index3i, BCase3i] {

  def dimens: Int = 3

  def corners: List[Index3i] =
    List(
      bmin, bmin.updateX(bmax.x), bmax.updateZ(bmin.z), bmin.updateY(bmax.y),
      bmin.updateZ(bmax.z), bmax.updateY(bmin.y), bmax, bmax.updateX(bmin.x)
    )

  def apply(i: Int): Index3i =
    i match {
      case 0 => bmin
      case 1 => bmax
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateMin(idx: Index3i): BCase3i = BCase3i(idx, bmax)
  def updateMax(idx: Index3i): BCase3i = BCase3i(bmin, idx)

  def update(i: Int, idx: Index3i): BCase3i =
    i match {
      case 0 => BCase3i(idx, bmax)
      case 1 => BCase3i(bmin, idx)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: BCase3i =>
        (that canEqual this) && (bmin == that.bmin) && (bmax == that.bmax)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: BCase3i => true
      case _             => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int =
    47 * (43 + bmin.hashCode) + bmax.hashCode

  def forall(p: (Index3i) => Boolean): Boolean = p(bmin) && p(bmax)
  def forall(that: BCase3i)(p: (Index3i, Index3i) => Boolean): Boolean =
    p(bmin, that.bmin) && p(bmax, that.bmax)

  def forany(p: (Index3i) => Boolean): Boolean = p(bmin) || p(bmax)
  def forany(that: BCase3i)(p: (Index3i, Index3i) => Boolean): Boolean =
    p(bmin, that.bmin) || p(bmax, that.bmax)

  def foreach(p: (Index3i) => Unit): Unit = { p(bmin); p(bmax) }

  def map(p: (Index3i) => Index3i): BCase3i = BCase3i(p(bmin), p(bmax))

  def reduce(p: (Index3i, Index3i) => Index3i): Index3i = p(bmin, bmax)

  def compwise(that: BCase3i)(p: (Index3i, Index3i) => Index3i): BCase3i =
    BCase3i(p(bmin, that.bmin), p(bmax, that.bmax))
  def min(that: BCase3i): BCase3i = compwise(that)(_ min _)
  def max(that: BCase3i): BCase3i = compwise(that)(_ max _)

  def area: Int = (1 /: range)(_ * _)

  def expand(idx: Index3i): BCase3i = BCase3i(idx min bmin, idx max bmax)

  def grow(delta: Index3i): BCase3i = BCase3i(bmin - delta, bmax + delta)

  def union(that: BCase3i): BCase3i = BCase3i(bmin min that.bmin, bmax max that.bmax)

  def range: Index3i = bmax - bmin + 1

  def clamp(idx: Index3i): Index3i = (idx max bmin) min bmax

  def split(dimen: Int): (BCase3i, BCase3i) = {
    val r = range(dimen)
    require(r % 2 == 0, "Cannot split in half along a direction with odd length!")
    val half = r / 2
    (updateMax(bmax.update(dimen, bmin(dimen) + half - 1)),
      updateMin(bmin.update(dimen, bmin(dimen) + half)))
  }
  def splitX: (BCase3i, BCase3i) = split(0)
  def splitY: (BCase3i, BCase3i) = split(1)
  def splitZ: (BCase3i, BCase3i) = split(2)

  def split(dimen: Int, idx: Int): (BCase3i, BCase3i) = {
    require((idx >= bmin(dimen)) && ((idx + 1) <= bmax(dimen)))
    (updateMax(bmax.update(dimen, idx)), updateMin(bmin.update(dimen, idx + 1)))
  }
  def splitX(idx: Int): (BCase3i, BCase3i) = split(0, idx)
  def splitY(idx: Int): (BCase3i, BCase3i) = split(1, idx)
  def splitZ(idx: Int): (BCase3i, BCase3i) = split(2, idx)

  def randomIndex(): Index3i =
    bmin + Index3i(Random.nextInt(range.x), Random.nextInt(range.y), Random.nextInt(range.z))
  def randomIndex(gen: Random): Index3i =
    bmin + Index3i(gen.nextInt(range.x), gen.nextInt(range.y), gen.nextInt(range.z))

  def isInside(idx: Index3i): Boolean = idx.forall(bmin)(_ >= _) && idx.forall(bmax)(_ <= _)
  def intersection(that: BCase3i): Option[BCase3i] = {
    val mn = bmin max that.bmin
    val mx = bmax min that.bmax
    if (!mn.forany(mx)(_ >= _)) Some(BCase3i(mn, mx)) else None
  }
  def intersects(that: BCase3i): Boolean =
    !((bmin max that.bmin).forany(bmax min that.bmax)(_ >= _))

  /** Convert to a String representation */
  override def toString: String = s"BCase3i$toPretty"

  def toPretty: String = s"(${bmin.toPretty}, ${bmax.toPretty})"

  def toBCase3i: BCase3i = this
  def toBCase2i: BCase2i = BCase2i(bmin.toIndex2i, bmax.toIndex2i)
  def toBCase1i: BCase1i = BCase1i(bmin.toIndex1i, bmax.toIndex1i)
}

object BCase3i {
  /** Create a new bounding box given two corner cell indices. */
  def apply(bmin: Index3i, bmax: Index3i): BCase3i = new BCase3i(bmin min bmax, bmin max bmax)

  def unapply(a: BCase3i): Some[(Index3i, Index3i)] = Some((a.bmin, a.bmax))

  /** Create a new bounding box which contains the given cell indices. */
  def apply(indices: Index3i*): BCase3i =
    if (indices.isEmpty) BCase3i(Index3i(0), Index3i(0))
    else indices.toArray.splitAt(1) match {
      case (Array(hd), tl) => new BCase3i((hd /: tl)(_ min _), (hd /: tl)(_ max _))
    }
}
