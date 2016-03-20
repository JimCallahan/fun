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

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.GL_UNSIGNED_BYTE
import org.lwjgl.opengl.GL11.GL_UNSIGNED_INT
import org.lwjgl.opengl.GL11.GL_UNSIGNED_SHORT
import org.lwjgl.opengl.GL15.GL_ELEMENT_ARRAY_BUFFER
import org.lwjgl.opengl.GL15.GL_STATIC_DRAW
import org.lwjgl.opengl.GL15.glBufferData
import org.lwjgl.opengl.GL15.glBufferSubData
import org.lwjgl.opengl.OpenGLException

import java.nio.ByteBuffer
import java.nio.IntBuffer
import java.nio.ShortBuffer

import GLUtil.tron

/** Index Buffer Object.
  * @constructor Create a new Index Buffer Object.
  * @param dataType The OpenGL storage type of the buffer.
  * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
  * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
  * GL_DYNAMIC_READ or GL_DYNAMIC_COPY. */
abstract class IndexBufferObject(val dataType: Int, val usage: Int)
    extends BufferObject(GL_ELEMENT_ARRAY_BUFFER, "IBO")

/** Factory for ByteIBO. */
object ByteIBO {
  /** Create a IBO to hold Byte index data.
    * @param numIndices The number of indices.
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads index data into the supplied buffer. */
  def apply(numIndices: Int, usage: Int = GL_STATIC_DRAW)(loader: ByteBuffer => Unit): ByteIBO = {
    val ibo = new ByteIBO(usage)
    val buf = BufferUtils.createByteBuffer(numIndices)
    loader(buf)
    buf.rewind
    ibo.load(buf)
    buf.clear
    ibo
  }
}

/** Vertex Buffer Object containing Byte attribute data. */
class ByteIBO(usage: Int)
    extends IndexBufferObject(GL_UNSIGNED_BYTE, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped.
    */
  private def load(data: ByteBuffer): Unit = {
    state match {
      case Reserved => {
        bind
        tron {
          glBufferData(target, data, usage)
        }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped.
    * @param offset The offset into server-side buffer where replacement will begin. */
  private def reload(data: ByteBuffer, offset: Long): Unit = {
    state match {
      case Loaded => {
        bind
        tron {
          glBufferSubData(target, offset, data)
        }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param numIndices The number of indices. (must be less-than-equal original buffer).
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads index data into the supplied buffer. */
  def reload(numIndices: Int, offset: Long)(loader: ByteBuffer => Unit): Unit = {
    val buf = BufferUtils.createByteBuffer(numIndices)
    loader(buf)
    buf.rewind
    reload(buf, offset)
    buf.clear
  }
}

/** Factory for ShortIBO. */
object ShortIBO {
  /** Create a IBO to hold Short index data.
    * @param numIndices The number of indices.
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads index data into the supplied buffer. */
  def apply(numIndices: Int, usage: Int = GL_STATIC_DRAW)(loader: ShortBuffer => Unit): ShortIBO = {
    val ibo = new ShortIBO(usage)
    val buf = BufferUtils.createShortBuffer(numIndices)
    loader(buf)
    buf.rewind
    ibo.load(buf)
    buf.clear
    ibo
  }
}

/** Vertex Buffer Object containing Short attribute data. */
class ShortIBO(usage: Int)
    extends IndexBufferObject(GL_UNSIGNED_SHORT, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(data: ShortBuffer): Unit = {
    state match {
      case Reserved => {
        bind
        tron {
          glBufferData(target, data, usage)
        }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped.
    * @param offset The offset into server-side buffer where replacement will begin. */
  private def reload(data: ShortBuffer, offset: Long): Unit = {
    state match {
      case Loaded => {
        bind
        tron {
          glBufferSubData(target, offset, data)
        }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param numIndices The number of indices. (must be less-than-equal original buffer).
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads index data into the supplied buffer. */
  def reload(numIndices: Int, offset: Long)(loader: ShortBuffer => Unit): Unit = {
    val buf = BufferUtils.createShortBuffer(numIndices)
    loader(buf)
    buf.rewind
    reload(buf, offset)
    buf.clear
  }
}

/** Factory for IntIBO. */
object IntIBO {
  /** Create a IBO to hold Int index data.
    * @param numIndices The number of indices.
    * @param usage Hint to OpenGL about the intended usage pattern: GL_STREAM_DRAW, GL_STREAM_READ,
    * GL_STREAM_COPY, GL_STATIC_DRAW, GL_STATIC_READ, GL_STATIC_COPY, GL_DYNAMIC_DRAW,
    * GL_DYNAMIC_READ or GL_DYNAMIC_COPY.
    * @param loader A function that loads index data into the supplied buffer. */
  def apply(numIndices: Int, usage: Int = GL_STATIC_DRAW)(loader: IntBuffer => Unit): IntIBO = {
    val ibo = new IntIBO(usage)
    val buf = BufferUtils.createIntBuffer(numIndices)
    loader(buf)
    buf.rewind
    ibo.load(buf)
    buf.clear
    ibo
  }
}

/** Vertex Buffer Object containing Int attribute data. */
class IntIBO(usage: Int)
    extends IndexBufferObject(GL_UNSIGNED_INT, usage) {

  /** Initial copy of client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped. */
  private def load(data: IntBuffer): Unit = {
    state match {
      case Reserved => {
        bind
        tron {
          glBufferData(target, data, usage)
        }
        unbind
        state = Loaded
      }
      case _ => throw new OpenGLException(
        "Unable to load client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param data The client data to be coped.
    * @param offset The offset into server-side buffer where replacement will begin. */
  private def reload(data: IntBuffer, offset: Long): Unit = {
    state match {
      case Loaded => {
        bind
        tron {
          glBufferSubData(target, offset, data)
        }
        unbind
      }
      case _ => throw new OpenGLException(
        "Unable to reload client data because the buffer object was in a " + state + " state!")
    }
  }

  /** Reload some or all client data to OpenGL server-side buffer storage.
    * @param numIndices The number of indices. (must be less-than-equal original buffer).
    * @param offset The offset into server-side buffer where replacement will begin.
    * @param loader A function that loads index data into the supplied buffer. */
  def reload(numIndices: Int, offset: Long)(loader: IntBuffer => Unit): Unit = {
    val buf = BufferUtils.createIntBuffer(numIndices)
    loader(buf)
    buf.rewind
    reload(buf, offset)
    buf.clear
  }
}
