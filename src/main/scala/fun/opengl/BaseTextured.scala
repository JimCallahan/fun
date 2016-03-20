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

import org.lwjgl.opengl.OpenGLException

/** The abstract base class of all OpenGL server-side objects associated with hardware texture units.
  * @constructor Create a new OpenGL Object.
  * @param The name for this type of OpenGL object. */
abstract class BaseTextured(objName: String)
    extends BaseStateful(objName) {

  /** Perform the OpenGL call which binds the specific type of object to a texture unit. */
  protected def bindGL(textureUnit: Int): Unit

  /** Bind the object to the given texture unit. */
  def bind(textureUnit: Int): Unit = {
    state match {
      case Reserved | Loaded => {
        bindGL(textureUnit)
      }
      case _ => throw new OpenGLException(
        "Unable to bind " + objName + " because it was in a " + state + " state!")
    }
  }

  /** Perform the OpenGL call which unbinds the specific type of object to a texture unit. */
  protected def unbindGL(textureUnit: Int): Unit

  /** Unbind the object from the given texture unit. */
  def unbind(textureUnit: Int): Unit = {
    state match {
      case Reserved | Loaded => {
        unbindGL(textureUnit)
      }
      case _ => throw new OpenGLException(
        "Unable to unbind " + objName + " because it was in a " + state + " state!")
    }
  }
}
