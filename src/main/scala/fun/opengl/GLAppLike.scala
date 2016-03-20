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

import fun.math.Vec2d

trait GLAppLike {
  /** Title to give the application. */
  def appName: String

  /** The dimensions of the display area. */
  def displaySize: Vec2d

  /** The number of bits to use for the stencil buffer (if any). */
  def stencilBits: Int

  /** Create the initial the graphics state. */
  def initial: GraphicsState

  /** Whether pressing ESC should exit the application immediately. */
  def enableEscToExit: Boolean = true
}
