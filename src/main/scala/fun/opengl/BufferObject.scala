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

import org.lwjgl.opengl.GL15.glBindBuffer
import org.lwjgl.opengl.GL15.glDeleteBuffers
import org.lwjgl.opengl.GL15.glGenBuffers

import GLUtil.tron

/** The abstract base class of all OpenGL server-side data buffer objects.
  * @constructor Create a new Buffer Object.
  * @param target The target (OpenGL enum) to which kind of buffer object is bound:
  * GL_ARRAY_BUFFER, GL_COPY_READ_BUFFER, GL_COPY_WRITE_BUFFER, GL_ELEMENT_ARRAY_BUFFER,
  * GL_PIXEL_PACK_BUFFER, GL_PIXEL_UNPACK_BUFFER, GL_TEXTURE_BUFFER, GL_TRANSFORM_FEEDBACK_BUFFER
  * or GL_UNIFORM_BUFFER.
  * @param The name for this type of OpenGL object. */
abstract class BufferObject(val target: Int, objName: String)
    extends BaseObject(objName) {

  /** The unique OpenGL handle for the buffer object.
    * Should not normally need to be accessed but made public so that JCuda can map the buffer. */
  val objectID: Int = {
    val id = tron { glGenBuffers }
    state = Reserved
    id
  }

  /** Perform the OpenGL call which binds the specific type of object. */
  protected def bindGL(): Unit =
    tron {
      glBindBuffer(target, objectID)
    }

  /** Perform the OpenGL call which unbinds the specific type of object. */
  protected def unbindGL(): Unit =
    tron {
      glBindBuffer(target, 0)
    }

  /** Perform the OpenGL call which releases all server-side resources. */
  protected def releaseGL(): Unit =
    tron {
      glDeleteBuffers(objectID)
    }
}
