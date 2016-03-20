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

/** The abstract base class of all OpenGL server-side objects.
  * @constructor Create a new OpenGL Object.
  * @param The name for this type of OpenGL object. */
abstract class BaseObject(objName: String)
    extends BaseStateful(objName) {

  /** Perform the OpenGL call which binds the specific type of object. */
  protected def bindGL(): Unit

  /** Bind the object to the current context. */
  def bind(): Unit = {
    state match {
      case Reserved | Loaded => {
        bindGL()
      }
      case _ => throw new OpenGLException(
        "Unable to bind " + objName + " because it was in a " + state + " state!")
    }
  }

  /** Perform the OpenGL call which unbinds the specific type of object. */
  protected def unbindGL(): Unit

  /** Unbind the object from the current context. */
  def unbind(): Unit = {
    state match {
      case Reserved | Loaded => {
        unbindGL()
      }
      case _ => throw new OpenGLException(
        "Unable to unbind " + objName + " because it was in a " + state + " state!")
    }
  }
}
