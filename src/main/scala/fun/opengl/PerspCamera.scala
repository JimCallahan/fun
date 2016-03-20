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

import fun.math.Matrix44d
import fun.math.Scalar
import fun.math.Vec2d
import fun.math.Vec4d

/** Factory for PerspCamera */
object PerspCamera {
  /** Create a perspective projection camera which fits the given display mode.
    * @param size The OpenGL display size (width, height).
    * @param fov The horizontal field of view (in degrees).
    * @param near The near clipping plane.
    * @param far The far clipping plane. */
  def apply(size: Vec2d, fov: Double, near: Double, far: Double): PerspCamera =
    new PerspCamera(fov, size.x / size.y, near, far)
}

/** A perspective projection camera.
  * @constructor Create a new camera.
  * @param fov The horizontal field of view (in degrees).
  * @param aspect The aspect ratio (width/height) of the frustum.
  * @param near The near clipping plane.
  * @param far The far clipping plane. */
class PerspCamera(val fov: Double, val aspect: Double, val near: Double, val far: Double) {

  /** The projection matrix which transforms from camera space to Normalized Device Coordinate
    * (NDC) space.
    */
  val project = {
    if (Scalar.equiv(aspect, 0.0) || (aspect < 0.0))
      throw new IllegalArgumentException("The aspect ratio must be positive!")
    if (Scalar.equiv(fov, 0.0) || (fov < 0.0) || (fov >= 180.0))
      throw new IllegalArgumentException(
        "The field of view must be positive and less than 180 degrees!")

    import scala.math.{ Pi, tan }
    val w = tan((fov * Pi) / 360.0)
    val r = near - far

    if (Scalar.equiv(r, 0.0))
      throw new IllegalArgumentException("The near and far clipping planes cannot be coincident!")

    Matrix44d(
      Vec4d(1.0 / (aspect * w), 0.0, 0.0, 0.0),
      Vec4d(0.0, 1.0 / aspect, 0.0, 0.0),
      Vec4d(0.0, 0.0, (far + near) / r, -1.0),
      Vec4d(0.0, 0.0, (2.0 * far * near) / r, 0.0))
  }

  /** Convert to a string representation. */
  override def toString() =
    "PerspCamera(" + fov + ", " + aspect + ", " + near + ", " + far + ")"
}
