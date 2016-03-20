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

/** A histogram of a set of values.
  * @param counts The number of values, when transformed into bucket space, lie inside each bucket interval.
  * @param intervals The bounds of each histogram bucket.
  * @param examples Some examples of values for each bucket.
  */
case class Histogram[A](
  counts: Vector[Long],
  intervals: Vector[Interval[Double]],
  examples: Vector[List[A]]
) {
  require(counts.size > 0, "The number of histgram buckets must be positive!")
  require(counts.size == intervals.size)

  /** Generate a string representation of the results suitable for reporting in log messages. */
  def report: String =
    counts.zip(intervals).zip(examples) map {
      case ((c, i), e) =>
        val estr = e match {
          case Nil => ""
          case es  => "(" + es.mkString(", ") + ")"
        }
        f"[${i.lower}%1.2f, ${i.upper}%1.2f) = $c  $estr"
    } mkString "\n"
}

object Histogram {
  /** Create a histogram of the given data in an arbitrary space.
    * @param data The data to bucket: (example, value)
    * @param interval The bounds of the buckets.
    * @param numBuckets The number of buckets to classify the data into.
    * @param numExamples The number of examples to record per bucket.
    * @param xform The transform from data space to bucket space.
    */
  def apply[A](data: Iterable[(A, Double)],
    interval: Interval[Double],
    numBuckets: Int,
    numExamples: Int,
    xform: Double => Double): Histogram[A] = {

    require(numBuckets > 0, "The number of histgram buckets must be positive!")
    require(interval.isLowerInclusive && !interval.isUpperInclusive)

    val Seq(mn, mx) = Seq(interval.lower, interval.upper) map xform
    val space = Space1d(Index1i(numBuckets), BBox1d(Pos1d(mn), Pos1d(mx)))
    val buckets = Array.fill(numBuckets)(0L)
    val filled = Array.fill(numBuckets)(numExamples <= 0)
    val examples = Array.fill(numBuckets)(List[A]())
    data foreach {
      case (e, v) =>
        val p = Pos1d(xform(v))
        val i = space.indexOf(p).x
        if ((i >= 0) && (i < numBuckets)) {
          buckets(i) += 1L
          if (!filled(i)) {
            examples(i) = e :: examples(i)
            if (examples(i).size >= numExamples)
              filled(i) = true
          }
        }
    }
    val intervals =
      for {
        i <- 0 until numBuckets
        bbox = space.cellBounds(Index1i(i))
      } yield Interval(bbox.bmin.x, bbox.bmax.x)

    Histogram(buckets.toVector, intervals.toVector, examples.toVector)
  }
}
