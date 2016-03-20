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
package fun.opengl

import fun.math.BBox2d
import fun.math.BBox3d
import fun.math.Matrix44d
import fun.math.Pos2d
import fun.math.Pos3d
import fun.math.Vec2d
import fun.math.Vec4d

/** Factory for OrthoCamera */
object OrthoCamera {
  /** Create an orthogonal projection camera.
    * @param bmin The (left, bottom, near) corner of the clipping box.
    * @param bmax The (right, top, far) corner of the clipping box. */
  def apply(bmin: Pos3d, bmax: Pos3d): OrthoCamera =
    new OrthoCamera(BBox3d(bmin, bmax))

  /** Create an orthogonal projection camera.
    * @param left The coordinate of the left clipping plane.
    * @param right The coordinate of the right clipping plane.
    * @param bottom The coordinate of the bottom clipping plane.
    * @param top The coordinate of the top clipping plane.
    * @param near The coordinate of the near clipping plane.
    * @param far The coordinate of the far clipping plane. */
  def apply(left: Double, right: Double,
    bottom: Double, top: Double,
    near: Double, far: Double): OrthoCamera =
    new OrthoCamera(BBox3d(Pos3d(left, bottom, near), Pos3d(right, top, far)))

  /** Create an orthogonal projection camera which fits the given display size so that the smaller
    * dimension will be mapped to [-1,1] range and the larger dimension proportionately.
    * @param size The OpenGL display size (width, height).
    * @param near The near clipping plane.
    * @param far The far clipping plane. */
  def apply(size: Vec2d, near: Double, far: Double): OrthoCamera = {
    val range =
      if (size.x < size.y)
        Vec2d(1.0, size.y / size.x)
      else
        Vec2d(size.x / size.y, 1.0)
    val bbox = BBox2d(Pos2d.origin - range, Pos2d.origin + range)

    new OrthoCamera(BBox3d(bbox.bmin.toPos3d.updateZ(near), bbox.bmax.toPos3d.updateZ(far)))
  }
}

/** An orthogonal projection camara.
  * @constructor Create a new camera.
  * @param bbox The bounding box in camera space which will be mapped to Normalized Device Coordinate
  * (NDC) space: [-1,1] in all dimensions. */
class OrthoCamera(val bbox: BBox3d) {

  /** The projection matrix which transforms from camera space to Normalized Device Coordinate
    * (NDC) space. */
  val project = {
    val r = bbox.range
    if (r.forany(_ <= 0.0))
      throw new IllegalArgumentException(
        "The viewing box must have a positive extent in all dimensions!")

    val t = (bbox.bmin + bbox.bmax.toVec3d) / r

    Matrix44d(
      Vec4d(2.0 / r.x, 0.0, 0.0, 0.0),
      Vec4d(0.0, 2.0 / r.y, 0.0, 0.0),
      Vec4d(0.0, 0.0, -2.0 / r.z, 0.0),
      Vec4d(-t.x, -t.y, -t.z, 1.0))
  }

  /** Convert to a string representation. */
  override def toString() =
    "OrthoCamera(" + bbox + ")"
}
