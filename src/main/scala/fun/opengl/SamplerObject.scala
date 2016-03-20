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

import org.lwjgl.opengl.GL11.GL_LINEAR
import org.lwjgl.opengl.GL11.GL_LINEAR_MIPMAP_LINEAR
import org.lwjgl.opengl.GL11.GL_TEXTURE_MAG_FILTER
import org.lwjgl.opengl.GL11.GL_TEXTURE_MIN_FILTER
import org.lwjgl.opengl.GL11.GL_TEXTURE_WRAP_S
import org.lwjgl.opengl.GL11.GL_TEXTURE_WRAP_T
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
import org.lwjgl.opengl.GL13.GL_TEXTURE0
import org.lwjgl.opengl.GL13.glActiveTexture
import org.lwjgl.opengl.GL33.glBindSampler
import org.lwjgl.opengl.GL33.glDeleteSamplers
import org.lwjgl.opengl.GL33.glGenSamplers
import org.lwjgl.opengl.GL33.glSamplerParameteri

import GLUtil.tron

/** An OpenGL texture sampler object. */
class SamplerObject
    extends BaseTextured("Sampler") {

  /** The unique OpenGL handle for the sampler object. */
  protected val objectID = {
    val id = tron { glGenSamplers }
    state = Reserved
    id
  }

  protected def bindGL(textureUnit: Int): Unit = {
    tron { glActiveTexture(GL_TEXTURE0 + textureUnit) }
    tron { glBindSampler(textureUnit, objectID) }
  }

  protected def unbindGL(textureUnit: Int): Unit = {
    tron { glActiveTexture(GL_TEXTURE0 + textureUnit) }
    tron { glBindSampler(textureUnit, 0) }
  }

  protected def releaseGL(): Unit =
    tron { glDeleteSamplers(objectID) }
}

object VideoSampler {
  /** Create an OpenGL sampler of video textures. */
  def apply(): VideoSampler = new VideoSampler()
}

/** An OpenGL sampler of video textures. */
class VideoSampler()
    extends SamplerObject {
  // Initialize sampler parameters.
  {
    tron { glSamplerParameteri(objectID, GL_TEXTURE_MAG_FILTER, GL_LINEAR) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_MIN_FILTER, GL_LINEAR) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE) }
  }
}

object MipmapSampler {
  /** Create an OpenGL sampler of mipmapped textures.
    * @param wrapS The wrap parameter (OpenGL enum) for the S-coordinate of the texture:
    * GL_CLAMP_TO_EDGE, GL_MIRRORED_REPEAT or GL_REPEAT.
    * @param wrapT The wrap parameter (OpenGL enum) for the T-coordinate of the texture:
    * GL_CLAMP_TO_EDGE, GL_MIRRORED_REPEAT or GL_REPEAT. */
  def apply(wrapS: Int, wrapT: Int): MipmapSampler = new MipmapSampler(wrapS, wrapT)
}

/** An OpenGL sampler of mipmapped textures. */
class MipmapSampler(val wrapS: Int, val wrapT: Int)
    extends SamplerObject {
  // Initialize sampler parameters.
  {
    tron { glSamplerParameteri(objectID, GL_TEXTURE_MAG_FILTER, GL_LINEAR) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_WRAP_S, wrapS) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_WRAP_T, wrapT) }
  }
}

object ClampedMipmapSampler {
  /** Create an OpenGL sampler of mipmapped textures which clamps texture coordinates. */
  def apply(): ClampedMipmapSampler = new ClampedMipmapSampler()
}

/** An OpenGL sampler of mipmapped textures which clamps texture coordinates.  */
class ClampedMipmapSampler
    extends MipmapSampler(GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE)

object VectorSampler {
  /** Create an OpenGL sampler of vector textures. */
  def apply(): VectorSampler = new VectorSampler()
}

/** An OpenGL sampler of vector textures. */
class VectorSampler
    extends SamplerObject {
  // Initialize sampler parameters.
  {
    tron { glSamplerParameteri(objectID, GL_TEXTURE_MAG_FILTER, GL_LINEAR) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_MIN_FILTER, GL_LINEAR) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE) }
    tron { glSamplerParameteri(objectID, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE) }
  }
}
