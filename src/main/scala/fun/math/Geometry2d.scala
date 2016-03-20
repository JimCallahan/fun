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

/** A collection of useful 2-dimensional geometric operations. */
object Geometry2d {

  /** Get the two vectors perpendicular to the given vector. */
  def perpendicular(v: Vec2d): (Vec2d, Vec2d) = (Vec2d(-v.y, v.x), Vec2d(v.y, -v.x))

  /** Find the single point (if possible) where the lines AB and BC intersect.
    * @return The intersection point or None if they are parallel or coincident.
    */
  def lineIntersect(a: Pos2d, b: Pos2d, c: Pos2d, d: Pos2d): Option[Pos2d] = {
    // Solve using determinants, see: http://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    val (Pos2d(x1, y1), Pos2d(x2, y2), Pos2d(x3, y3), Pos2d(x4, y4)) = (a, b, c, d)
    val denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    if (Scalar.equiv(denom, 0.0)) None
    else {
      val j = x1 * y2 - y1 * x2
      val k = x3 * y4 - y3 * x4
      val px = (j * (x3 - x4) - (x1 - x2) * k) / denom
      val py = (j * (y3 - y4) - (y1 - y2) * k) / denom
      val p = Pos2d(px, py)
      if (p.isNaN) None else Some(p)
    }
  }
}
