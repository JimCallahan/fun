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
import fun.math.Matrix44d
import fun.math.Pos2d
import fun.math.Vec2d
import fun.math.Vec3d

/** A 2D orthogonal camera which can be positioned and zoomed.
  * @constructor Create a new camera.
  * @param camera The 3D orthogonal view transformation.
  * @param zoom The level of zoom (power of 2).
  * @param pos The camera viewing position.
  * @param factor The scaling factor from window to camera space.
  * @param The pixel resolution of the display device.
  */
case class ZoomCamera(camera: OrthoCamera, zoom: Double, pos: Pos2d, factor: Vec2d, res: Vec2d) {
  /** The current camera scaling factor based on zoom level. */
  val scale = scala.math.pow(2.0, zoom)

  /** The bounding box of the viewable area in world space. */
  val bbox = camera.bbox.toBBox2d.map((c: Pos2d) => (pos * -1.0) + (c.toVec2d / scale))

  /** The combined view and projection transformation. */
  val xform =
    (camera.project *
      Matrix44d.scale(Vec3d(scale, scale, 1.0)) *
      Matrix44d.translate(Vec3d(pos.x, pos.y, 0.0)))

  /** Position and zoom the camera so that the given bounding box is viewable. */
  def view(bbox: BBox2d): ZoomCamera = {
    import scala.math.log
    val range = bbox.range
    val scale = 2.0 / ((range.x max range.y) * 1.2)
    val nzoom = log(scale) / log(2.0)
    copy(pos = -bbox.center - Vec2d(0.0, 0.1 / scale), zoom = nzoom)
  }
}

/** Companion object for ZoomCamera. */
object ZoomCamera {
  /** Create an initial camera.
    * @param size The OpenGL window (width, height).
    * @param zoom The level of zoom (power of 2).
    */
  def apply(size: Vec2d, zoom: Double): ZoomCamera = {
    val camera = OrthoCamera(size, -1.0, 1.0)
    val factor = camera.bbox.range.toVec2d / size
    ZoomCamera(camera, zoom, Pos2d(0.0), factor, size)
  }
}
