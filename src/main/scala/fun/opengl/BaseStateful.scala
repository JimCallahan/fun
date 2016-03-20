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

/** The abstract base class of all OpenGL server-side objects which maintain state.
  * @param The name for this type of OpenGL object. */
abstract class BaseStateful(objName: String) {

  /** The state of an generic OpenGL object */
  sealed trait ObjState

  /** Initial state of the object before any OpenGL calls are made. */
  case object Initial extends ObjState

  /** The object handle has been reserved, but no data has been allocated or copied. */
  case object Reserved extends ObjState

  /** The server-side data has been allocated and at least an initial set of client values
    * copied so that the object is ready for use.
    */
  case object Loaded extends ObjState

  /** The server-side storage has been released (deleted) leaving this instance useless. */
  case object Released extends ObjState

  /** State of the OpenGL server-side object. */
  protected var state: ObjState = Initial

  /** Whether the object is ready for use. */
  def isValid(): Boolean =
    state match {
      case Loaded => true
      case _      => false
    }

  /** Perform the OpenGL call which releases all server-side resources. */
  protected def releaseGL(): Unit

  /** Release all OpenGL server-side resources. */
  def release(): Unit = {
    state match {
      case Loaded => {
        releaseGL()
        state = Released
      }
      case _ => throw new OpenGLException(
        "Unable to release " + objName + " because it was in a " + state + " state!")
    }
  }
}
