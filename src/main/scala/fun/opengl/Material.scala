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

import scala.collection.immutable.Map

/** The combined OpenGL shader programs and textures needed to render a set of geometry
  * with the same appearance.
  * @constructor Create a new Material.
  * @param program The shader program used to render geometry with this material.
  * @param textures The mapping of texture/sampler name registered with the TextureMgr to OpenGL
  * shader program uniform name for the sampler. */
class Material(val program: ShaderProgram, val textures: Map[String, String]) {

  /** Bind the textures to the program texture uniforms to prepare for rendering. */
  def bind(): Unit = {
    program.use
    val texUnits = TextureMgr.bind(textures.keySet)
    for ((n, u) <- textures)
      program.setUniform(u, texUnits(n))
  }
}

object Material {
  /** Create a new Material.
    * @param program The shader program used to render geometry with this material.
    * @param textures The mapping of texture/sampler name registered with the TextureMgr to OpenGL
    * shader program uniform name for the sampler. */
  def apply(program: ShaderProgram, textures: Map[String, String]): Material =
    new Material(program, textures)
}
