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
package fun.math

import java.nio.CharBuffer
import java.nio.DoubleBuffer
import java.nio.FloatBuffer
import java.nio.IntBuffer
import java.nio.LongBuffer
import java.nio.ShortBuffer

import scala.annotation.tailrec
import scala.util.Random

class Vec4d private (val x: Double, val y: Double, val z: Double, val w: Double)
    extends Tuple4[Double, Vec4d]
    with Swizzle4[Vec2d, Vec3d, Vec4d]
    with DoubleTupleOps[Vec4d]
    with VecOps[Double, Vec4d]
    with Vec {

  /** Compares this vector to the specified value for equality. */
  override def equals(that: Any): Boolean =
    that match {
      case that: Vec4d =>
        (that canEqual this) &&
        Scalar.equiv(x, that.x) &&
        Scalar.equiv(y, that.y) &&
        Scalar.equiv(z, that.z) &&
        Scalar.equiv(w, that.w)
      case _ => false
    }

  /** A method that should be called from every well-designed equals method that is open
    * to be overridden in a subclass.
    */
  def canEqual(that: Any): Boolean =
    that match {
      case that: Vec4d => true
      case _           => false
    }

  /** Returns a hash code value for the object. */
  override def hashCode: Int = 53 * (47 * (43 * (41 + x.##) + y.##) + z.##) + w.##

  def apply(i: Int): Double =
    i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def update(i: Int, e: Double): Vec4d =
    i match {
      case 0 => Vec4d(e, y, z, w)
      case 1 => Vec4d(x, e, z, w)
      case 2 => Vec4d(x, y, e, w)
      case 3 => Vec4d(x, y, z, e)
      case _ => throw new IllegalArgumentException("Invalid index (" + i + ")!")
    }

  def updateX(e: Double): Vec4d = Vec4d(e, y, z, w)
  def updateY(e: Double): Vec4d = Vec4d(x, e, z, w)
  def updateZ(e: Double): Vec4d = Vec4d(x, y, e, w)
  def updateW(e: Double): Vec4d = Vec4d(x, y, z, e)

  def xx: Vec2d = Vec2d(x, x)
  def xy: Vec2d = Vec2d(x, y)
  def yx: Vec2d = Vec2d(y, x)
  def xz: Vec2d = Vec2d(x, z)
  def zx: Vec2d = Vec2d(z, x)
  def xw: Vec2d = Vec2d(x, w)
  def wx: Vec2d = Vec2d(w, x)
  def yy: Vec2d = Vec2d(y, y)
  def yz: Vec2d = Vec2d(y, z)
  def zy: Vec2d = Vec2d(z, y)
  def yw: Vec2d = Vec2d(y, w)
  def wy: Vec2d = Vec2d(w, y)
  def zz: Vec2d = Vec2d(z, z)
  def zw: Vec2d = Vec2d(z, w)
  def wz: Vec2d = Vec2d(w, z)
  def ww: Vec2d = Vec2d(w, w)

  def xxx: Vec3d = Vec3d(x, x, x)
  def xxy: Vec3d = Vec3d(x, x, y)
  def xyx: Vec3d = Vec3d(x, y, x)
  def yxx: Vec3d = Vec3d(y, x, x)
  def xxz: Vec3d = Vec3d(x, x, z)
  def xzx: Vec3d = Vec3d(x, z, x)
  def zxx: Vec3d = Vec3d(z, x, x)
  def xxw: Vec3d = Vec3d(x, x, w)
  def xwx: Vec3d = Vec3d(x, w, x)
  def wxx: Vec3d = Vec3d(w, x, x)
  def xyy: Vec3d = Vec3d(x, y, y)
  def yxy: Vec3d = Vec3d(y, x, y)
  def yyx: Vec3d = Vec3d(y, y, x)
  def xyz: Vec3d = Vec3d(x, y, z)
  def xzy: Vec3d = Vec3d(x, z, y)
  def yxz: Vec3d = Vec3d(y, x, z)
  def yzx: Vec3d = Vec3d(y, z, x)
  def zxy: Vec3d = Vec3d(z, x, y)
  def zyx: Vec3d = Vec3d(z, y, x)
  def xyw: Vec3d = Vec3d(x, y, w)
  def xwy: Vec3d = Vec3d(x, w, y)
  def yxw: Vec3d = Vec3d(y, x, w)
  def ywx: Vec3d = Vec3d(y, w, x)
  def wxy: Vec3d = Vec3d(w, x, y)
  def wyx: Vec3d = Vec3d(w, y, x)
  def xzz: Vec3d = Vec3d(x, z, z)
  def zxz: Vec3d = Vec3d(z, x, z)
  def zzx: Vec3d = Vec3d(z, z, x)
  def xzw: Vec3d = Vec3d(x, z, w)
  def xwz: Vec3d = Vec3d(x, w, z)
  def zxw: Vec3d = Vec3d(z, x, w)
  def zwx: Vec3d = Vec3d(z, w, x)
  def wxz: Vec3d = Vec3d(w, x, z)
  def wzx: Vec3d = Vec3d(w, z, x)
  def xww: Vec3d = Vec3d(x, w, w)
  def wxw: Vec3d = Vec3d(w, x, w)
  def wwx: Vec3d = Vec3d(w, w, x)
  def yyy: Vec3d = Vec3d(y, y, y)
  def yyz: Vec3d = Vec3d(y, y, z)
  def yzy: Vec3d = Vec3d(y, z, y)
  def zyy: Vec3d = Vec3d(z, y, y)
  def yyw: Vec3d = Vec3d(y, y, w)
  def ywy: Vec3d = Vec3d(y, w, y)
  def wyy: Vec3d = Vec3d(w, y, y)
  def yzz: Vec3d = Vec3d(y, z, z)
  def zyz: Vec3d = Vec3d(z, y, z)
  def zzy: Vec3d = Vec3d(z, z, y)
  def yzw: Vec3d = Vec3d(y, z, w)
  def ywz: Vec3d = Vec3d(y, w, z)
  def zyw: Vec3d = Vec3d(z, y, w)
  def zwy: Vec3d = Vec3d(z, w, y)
  def wyz: Vec3d = Vec3d(w, y, z)
  def wzy: Vec3d = Vec3d(w, z, y)
  def yww: Vec3d = Vec3d(y, w, w)
  def wyw: Vec3d = Vec3d(w, y, w)
  def wwy: Vec3d = Vec3d(w, w, y)
  def zzz: Vec3d = Vec3d(z, z, z)
  def zzw: Vec3d = Vec3d(z, z, w)
  def zwz: Vec3d = Vec3d(z, w, z)
  def wzz: Vec3d = Vec3d(w, z, z)
  def zww: Vec3d = Vec3d(z, w, w)
  def wzw: Vec3d = Vec3d(w, z, w)
  def wwz: Vec3d = Vec3d(w, w, z)
  def www: Vec3d = Vec3d(w, w, w)

  def xxxx: Vec4d = Vec4d(x, x, x, x)
  def xxxy: Vec4d = Vec4d(x, x, x, y)
  def xxyx: Vec4d = Vec4d(x, x, y, x)
  def xyxx: Vec4d = Vec4d(x, y, x, x)
  def yxxx: Vec4d = Vec4d(y, x, x, x)
  def xxxz: Vec4d = Vec4d(x, x, x, z)
  def xxzx: Vec4d = Vec4d(x, x, z, x)
  def xzxx: Vec4d = Vec4d(x, z, x, x)
  def zxxx: Vec4d = Vec4d(z, x, x, x)
  def xxxw: Vec4d = Vec4d(x, x, x, w)
  def xxwx: Vec4d = Vec4d(x, x, w, x)
  def xwxx: Vec4d = Vec4d(x, w, x, x)
  def wxxx: Vec4d = Vec4d(w, x, x, x)
  def xxyy: Vec4d = Vec4d(x, x, y, y)
  def xyxy: Vec4d = Vec4d(x, y, x, y)
  def xyyx: Vec4d = Vec4d(x, y, y, x)
  def yxxy: Vec4d = Vec4d(y, x, x, y)
  def yxyx: Vec4d = Vec4d(y, x, y, x)
  def yyxx: Vec4d = Vec4d(y, y, x, x)
  def xxyz: Vec4d = Vec4d(x, x, y, z)
  def xxzy: Vec4d = Vec4d(x, x, z, y)
  def xyxz: Vec4d = Vec4d(x, y, x, z)
  def xyzx: Vec4d = Vec4d(x, y, z, x)
  def xzxy: Vec4d = Vec4d(x, z, x, y)
  def xzyx: Vec4d = Vec4d(x, z, y, x)
  def yxxz: Vec4d = Vec4d(y, x, x, z)
  def yxzx: Vec4d = Vec4d(y, x, z, x)
  def yzxx: Vec4d = Vec4d(y, z, x, x)
  def zxxy: Vec4d = Vec4d(z, x, x, y)
  def zxyx: Vec4d = Vec4d(z, x, y, x)
  def zyxx: Vec4d = Vec4d(z, y, x, x)
  def xxyw: Vec4d = Vec4d(x, x, y, w)
  def xxwy: Vec4d = Vec4d(x, x, w, y)
  def xyxw: Vec4d = Vec4d(x, y, x, w)
  def xywx: Vec4d = Vec4d(x, y, w, x)
  def xwxy: Vec4d = Vec4d(x, w, x, y)
  def xwyx: Vec4d = Vec4d(x, w, y, x)
  def yxxw: Vec4d = Vec4d(y, x, x, w)
  def yxwx: Vec4d = Vec4d(y, x, w, x)
  def ywxx: Vec4d = Vec4d(y, w, x, x)
  def wxxy: Vec4d = Vec4d(w, x, x, y)
  def wxyx: Vec4d = Vec4d(w, x, y, x)
  def wyxx: Vec4d = Vec4d(w, y, x, x)
  def xxzz: Vec4d = Vec4d(x, x, z, z)
  def xzxz: Vec4d = Vec4d(x, z, x, z)
  def xzzx: Vec4d = Vec4d(x, z, z, x)
  def zxxz: Vec4d = Vec4d(z, x, x, z)
  def zxzx: Vec4d = Vec4d(z, x, z, x)
  def zzxx: Vec4d = Vec4d(z, z, x, x)
  def xxzw: Vec4d = Vec4d(x, x, z, w)
  def xxwz: Vec4d = Vec4d(x, x, w, z)
  def xzxw: Vec4d = Vec4d(x, z, x, w)
  def xzwx: Vec4d = Vec4d(x, z, w, x)
  def xwxz: Vec4d = Vec4d(x, w, x, z)
  def xwzx: Vec4d = Vec4d(x, w, z, x)
  def zxxw: Vec4d = Vec4d(z, x, x, w)
  def zxwx: Vec4d = Vec4d(z, x, w, x)
  def zwxx: Vec4d = Vec4d(z, w, x, x)
  def wxxz: Vec4d = Vec4d(w, x, x, z)
  def wxzx: Vec4d = Vec4d(w, x, z, x)
  def wzxx: Vec4d = Vec4d(w, z, x, x)
  def xxww: Vec4d = Vec4d(x, x, w, w)
  def xwxw: Vec4d = Vec4d(x, w, x, w)
  def xwwx: Vec4d = Vec4d(x, w, w, x)
  def wxxw: Vec4d = Vec4d(w, x, x, w)
  def wxwx: Vec4d = Vec4d(w, x, w, x)
  def wwxx: Vec4d = Vec4d(w, w, x, x)
  def xyyy: Vec4d = Vec4d(x, y, y, y)
  def yxyy: Vec4d = Vec4d(y, x, y, y)
  def yyxy: Vec4d = Vec4d(y, y, x, y)
  def yyyx: Vec4d = Vec4d(y, y, y, x)
  def xyyz: Vec4d = Vec4d(x, y, y, z)
  def xyzy: Vec4d = Vec4d(x, y, z, y)
  def xzyy: Vec4d = Vec4d(x, z, y, y)
  def yxyz: Vec4d = Vec4d(y, x, y, z)
  def yxzy: Vec4d = Vec4d(y, x, z, y)
  def yyxz: Vec4d = Vec4d(y, y, x, z)
  def yyzx: Vec4d = Vec4d(y, y, z, x)
  def yzxy: Vec4d = Vec4d(y, z, x, y)
  def yzyx: Vec4d = Vec4d(y, z, y, x)
  def zxyy: Vec4d = Vec4d(z, x, y, y)
  def zyxy: Vec4d = Vec4d(z, y, x, y)
  def zyyx: Vec4d = Vec4d(z, y, y, x)
  def xyyw: Vec4d = Vec4d(x, y, y, w)
  def xywy: Vec4d = Vec4d(x, y, w, y)
  def xwyy: Vec4d = Vec4d(x, w, y, y)
  def yxyw: Vec4d = Vec4d(y, x, y, w)
  def yxwy: Vec4d = Vec4d(y, x, w, y)
  def yyxw: Vec4d = Vec4d(y, y, x, w)
  def yywx: Vec4d = Vec4d(y, y, w, x)
  def ywxy: Vec4d = Vec4d(y, w, x, y)
  def ywyx: Vec4d = Vec4d(y, w, y, x)
  def wxyy: Vec4d = Vec4d(w, x, y, y)
  def wyxy: Vec4d = Vec4d(w, y, x, y)
  def wyyx: Vec4d = Vec4d(w, y, y, x)
  def xyzz: Vec4d = Vec4d(x, y, z, z)
  def xzyz: Vec4d = Vec4d(x, z, y, z)
  def xzzy: Vec4d = Vec4d(x, z, z, y)
  def yxzz: Vec4d = Vec4d(y, x, z, z)
  def yzxz: Vec4d = Vec4d(y, z, x, z)
  def yzzx: Vec4d = Vec4d(y, z, z, x)
  def zxyz: Vec4d = Vec4d(z, x, y, z)
  def zxzy: Vec4d = Vec4d(z, x, z, y)
  def zyxz: Vec4d = Vec4d(z, y, x, z)
  def zyzx: Vec4d = Vec4d(z, y, z, x)
  def zzxy: Vec4d = Vec4d(z, z, x, y)
  def zzyx: Vec4d = Vec4d(z, z, y, x)
  def xyzw: Vec4d = Vec4d(x, y, z, w)
  def xywz: Vec4d = Vec4d(x, y, w, z)
  def xzyw: Vec4d = Vec4d(x, z, y, w)
  def xzwy: Vec4d = Vec4d(x, z, w, y)
  def xwyz: Vec4d = Vec4d(x, w, y, z)
  def xwzy: Vec4d = Vec4d(x, w, z, y)
  def yxzw: Vec4d = Vec4d(y, x, z, w)
  def yxwz: Vec4d = Vec4d(y, x, w, z)
  def yzxw: Vec4d = Vec4d(y, z, x, w)
  def yzwx: Vec4d = Vec4d(y, z, w, x)
  def ywxz: Vec4d = Vec4d(y, w, x, z)
  def ywzx: Vec4d = Vec4d(y, w, z, x)
  def zxyw: Vec4d = Vec4d(z, x, y, w)
  def zxwy: Vec4d = Vec4d(z, x, w, y)
  def zyxw: Vec4d = Vec4d(z, y, x, w)
  def zywx: Vec4d = Vec4d(z, y, w, x)
  def zwxy: Vec4d = Vec4d(z, w, x, y)
  def zwyx: Vec4d = Vec4d(z, w, y, x)
  def wxyz: Vec4d = Vec4d(w, x, y, z)
  def wxzy: Vec4d = Vec4d(w, x, z, y)
  def wyxz: Vec4d = Vec4d(w, y, x, z)
  def wyzx: Vec4d = Vec4d(w, y, z, x)
  def wzxy: Vec4d = Vec4d(w, z, x, y)
  def wzyx: Vec4d = Vec4d(w, z, y, x)
  def xyww: Vec4d = Vec4d(x, y, w, w)
  def xwyw: Vec4d = Vec4d(x, w, y, w)
  def xwwy: Vec4d = Vec4d(x, w, w, y)
  def yxww: Vec4d = Vec4d(y, x, w, w)
  def ywxw: Vec4d = Vec4d(y, w, x, w)
  def ywwx: Vec4d = Vec4d(y, w, w, x)
  def wxyw: Vec4d = Vec4d(w, x, y, w)
  def wxwy: Vec4d = Vec4d(w, x, w, y)
  def wyxw: Vec4d = Vec4d(w, y, x, w)
  def wywx: Vec4d = Vec4d(w, y, w, x)
  def wwxy: Vec4d = Vec4d(w, w, x, y)
  def wwyx: Vec4d = Vec4d(w, w, y, x)
  def xzzz: Vec4d = Vec4d(x, z, z, z)
  def zxzz: Vec4d = Vec4d(z, x, z, z)
  def zzxz: Vec4d = Vec4d(z, z, x, z)
  def zzzx: Vec4d = Vec4d(z, z, z, x)
  def xzzw: Vec4d = Vec4d(x, z, z, w)
  def xzwz: Vec4d = Vec4d(x, z, w, z)
  def xwzz: Vec4d = Vec4d(x, w, z, z)
  def zxzw: Vec4d = Vec4d(z, x, z, w)
  def zxwz: Vec4d = Vec4d(z, x, w, z)
  def zzxw: Vec4d = Vec4d(z, z, x, w)
  def zzwx: Vec4d = Vec4d(z, z, w, x)
  def zwxz: Vec4d = Vec4d(z, w, x, z)
  def zwzx: Vec4d = Vec4d(z, w, z, x)
  def wxzz: Vec4d = Vec4d(w, x, z, z)
  def wzxz: Vec4d = Vec4d(w, z, x, z)
  def wzzx: Vec4d = Vec4d(w, z, z, x)
  def xzww: Vec4d = Vec4d(x, z, w, w)
  def xwzw: Vec4d = Vec4d(x, w, z, w)
  def xwwz: Vec4d = Vec4d(x, w, w, z)
  def zxww: Vec4d = Vec4d(z, x, w, w)
  def zwxw: Vec4d = Vec4d(z, w, x, w)
  def zwwx: Vec4d = Vec4d(z, w, w, x)
  def wxzw: Vec4d = Vec4d(w, x, z, w)
  def wxwz: Vec4d = Vec4d(w, x, w, z)
  def wzxw: Vec4d = Vec4d(w, z, x, w)
  def wzwx: Vec4d = Vec4d(w, z, w, x)
  def wwxz: Vec4d = Vec4d(w, w, x, z)
  def wwzx: Vec4d = Vec4d(w, w, z, x)
  def xwww: Vec4d = Vec4d(x, w, w, w)
  def wxww: Vec4d = Vec4d(w, x, w, w)
  def wwxw: Vec4d = Vec4d(w, w, x, w)
  def wwwx: Vec4d = Vec4d(w, w, w, x)
  def yyyy: Vec4d = Vec4d(y, y, y, y)
  def yyyz: Vec4d = Vec4d(y, y, y, z)
  def yyzy: Vec4d = Vec4d(y, y, z, y)
  def yzyy: Vec4d = Vec4d(y, z, y, y)
  def zyyy: Vec4d = Vec4d(z, y, y, y)
  def yyyw: Vec4d = Vec4d(y, y, y, w)
  def yywy: Vec4d = Vec4d(y, y, w, y)
  def ywyy: Vec4d = Vec4d(y, w, y, y)
  def wyyy: Vec4d = Vec4d(w, y, y, y)
  def yyzz: Vec4d = Vec4d(y, y, z, z)
  def yzyz: Vec4d = Vec4d(y, z, y, z)
  def yzzy: Vec4d = Vec4d(y, z, z, y)
  def zyyz: Vec4d = Vec4d(z, y, y, z)
  def zyzy: Vec4d = Vec4d(z, y, z, y)
  def zzyy: Vec4d = Vec4d(z, z, y, y)
  def yyzw: Vec4d = Vec4d(y, y, z, w)
  def yywz: Vec4d = Vec4d(y, y, w, z)
  def yzyw: Vec4d = Vec4d(y, z, y, w)
  def yzwy: Vec4d = Vec4d(y, z, w, y)
  def ywyz: Vec4d = Vec4d(y, w, y, z)
  def ywzy: Vec4d = Vec4d(y, w, z, y)
  def zyyw: Vec4d = Vec4d(z, y, y, w)
  def zywy: Vec4d = Vec4d(z, y, w, y)
  def zwyy: Vec4d = Vec4d(z, w, y, y)
  def wyyz: Vec4d = Vec4d(w, y, y, z)
  def wyzy: Vec4d = Vec4d(w, y, z, y)
  def wzyy: Vec4d = Vec4d(w, z, y, y)
  def yyww: Vec4d = Vec4d(y, y, w, w)
  def ywyw: Vec4d = Vec4d(y, w, y, w)
  def ywwy: Vec4d = Vec4d(y, w, w, y)
  def wyyw: Vec4d = Vec4d(w, y, y, w)
  def wywy: Vec4d = Vec4d(w, y, w, y)
  def wwyy: Vec4d = Vec4d(w, w, y, y)
  def yzzz: Vec4d = Vec4d(y, z, z, z)
  def zyzz: Vec4d = Vec4d(z, y, z, z)
  def zzyz: Vec4d = Vec4d(z, z, y, z)
  def zzzy: Vec4d = Vec4d(z, z, z, y)
  def yzzw: Vec4d = Vec4d(y, z, z, w)
  def yzwz: Vec4d = Vec4d(y, z, w, z)
  def ywzz: Vec4d = Vec4d(y, w, z, z)
  def zyzw: Vec4d = Vec4d(z, y, z, w)
  def zywz: Vec4d = Vec4d(z, y, w, z)
  def zzyw: Vec4d = Vec4d(z, z, y, w)
  def zzwy: Vec4d = Vec4d(z, z, w, y)
  def zwyz: Vec4d = Vec4d(z, w, y, z)
  def zwzy: Vec4d = Vec4d(z, w, z, y)
  def wyzz: Vec4d = Vec4d(w, y, z, z)
  def wzyz: Vec4d = Vec4d(w, z, y, z)
  def wzzy: Vec4d = Vec4d(w, z, z, y)
  def yzww: Vec4d = Vec4d(y, z, w, w)
  def ywzw: Vec4d = Vec4d(y, w, z, w)
  def ywwz: Vec4d = Vec4d(y, w, w, z)
  def zyww: Vec4d = Vec4d(z, y, w, w)
  def zwyw: Vec4d = Vec4d(z, w, y, w)
  def zwwy: Vec4d = Vec4d(z, w, w, y)
  def wyzw: Vec4d = Vec4d(w, y, z, w)
  def wywz: Vec4d = Vec4d(w, y, w, z)
  def wzyw: Vec4d = Vec4d(w, z, y, w)
  def wzwy: Vec4d = Vec4d(w, z, w, y)
  def wwyz: Vec4d = Vec4d(w, w, y, z)
  def wwzy: Vec4d = Vec4d(w, w, z, y)
  def ywww: Vec4d = Vec4d(y, w, w, w)
  def wyww: Vec4d = Vec4d(w, y, w, w)
  def wwyw: Vec4d = Vec4d(w, w, y, w)
  def wwwy: Vec4d = Vec4d(w, w, w, y)
  def zzzz: Vec4d = Vec4d(z, z, z, z)
  def zzzw: Vec4d = Vec4d(z, z, z, w)
  def zzwz: Vec4d = Vec4d(z, z, w, z)
  def zwzz: Vec4d = Vec4d(z, w, z, z)
  def wzzz: Vec4d = Vec4d(w, z, z, z)
  def zzww: Vec4d = Vec4d(z, z, w, w)
  def zwzw: Vec4d = Vec4d(z, w, z, w)
  def zwwz: Vec4d = Vec4d(z, w, w, z)
  def wzzw: Vec4d = Vec4d(w, z, z, w)
  def wzwz: Vec4d = Vec4d(w, z, w, z)
  def wwzz: Vec4d = Vec4d(w, w, z, z)
  def zwww: Vec4d = Vec4d(z, w, w, w)
  def wzww: Vec4d = Vec4d(w, z, w, w)
  def wwzw: Vec4d = Vec4d(w, w, z, w)
  def wwwz: Vec4d = Vec4d(w, w, w, z)
  def wwww: Vec4d = Vec4d(w, w, w, w)

  def magSq: Double = dot(this)
  def mag: Double = scala.math.sqrt(magSq)
  def normalized: Vec4d = {
    val m = mag
    if (Scalar.equiv(m, 0.0)) Vec4d(0.0)
    else (this / m)
  }

  def dot(that: Vec4d): Double = x * that.x + y * that.y + z * that.z + w * that.w

  def unary_- : Vec4d = Vec4d(-x, -y, -z, -w)

  def +(that: Vec4d): Vec4d = Vec4d(x + that.x, y + that.y, z + that.z, w + that.w)
  def -(that: Vec4d): Vec4d = Vec4d(x - that.x, y - that.y, z - that.z, w - that.w)
  def *(that: Vec4d): Vec4d = Vec4d(x * that.x, y * that.y, z * that.z, w * that.w)
  def /(that: Vec4d): Vec4d = Vec4d(x / that.x, y / that.y, z / that.z, w / that.w)

  def +(s: Double): Vec4d = Vec4d(x + s, y + s, z + s, w + s)
  def -(s: Double): Vec4d = Vec4d(x - s, y - s, z - s, w - s)
  def *(s: Double): Vec4d = Vec4d(x * s, y * s, z * s, w * s)
  def /(s: Double): Vec4d = Vec4d(x / s, y / s, z / s, w / s)

  def forall(p: (Double) => Boolean): Boolean = p(x) && p(y) && p(z) && p(w)
  def forall(that: Vec4d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) && p(y, that.y) && p(z, that.z) && p(w, that.w)
  def equiv(that: Vec4d, epsilon: Double): Boolean = forall(that)(Scalar.equiv(_, _, epsilon))
  def equiv(that: Vec4d): Boolean = forall(that)(Scalar.equiv(_, _))

  def isNaN: Boolean = x.isNaN || y.isNaN || z.isNaN || w.isNaN

  def forany(p: (Double) => Boolean): Boolean = p(x) || p(y) || p(z) || p(w)
  def forany(that: Vec4d)(p: (Double, Double) => Boolean): Boolean = p(x, that.x) || p(y, that.y) || p(z, that.z) || p(w, that.w)

  def foreach(p: (Double) => Unit): Unit = { p(x); p(y); p(z); p(w) }

  def map(p: (Double) => Double): Vec4d = Vec4d(p(x), p(y), p(z), p(w))

  def foldLeft[A](start: A)(f: (A, Double) => A): A = f(f(f(f(start, x), y), z), w)
  def /:[A](start: A)(f: (A, Double) => A): A = foldLeft(start)(f)

  def foldRight[A](start: A)(f: (Double, A) => A): A = f(x, f(y, f(z, f(w, start))))
  def :\[A](start: A)(f: (Double, A) => A): A = foldRight(start)(f)

  def reduce(p: (Double, Double) => Double): Double = p(x, p(y, p(z, w)))
  def min: Double = reduce(_ min _)
  def max: Double = reduce(_ max _)

  def compwise(that: Vec4d)(p: (Double, Double) => Double): Vec4d =
    Vec4d(p(x, that.x), p(y, that.y), p(z, that.z), p(w, that.w))
  def min(that: Vec4d): Vec4d = compwise(that)(_ min _)
  def max(that: Vec4d): Vec4d = compwise(that)(_ max _)
  def lerp(that: Vec4d, t: Double): Vec4d = compwise(that)(Scalar.lerp(_, _, t))
  def smoothlerp(that: Vec4d, t: Double): Vec4d = compwise(that)(Scalar.smoothlerp(_, _, t))

  def compwise(a: Vec4d, b: Vec4d)(p: (Double, Double, Double) => Double): Vec4d =
    Vec4d(p(x, a.x, b.x), p(y, a.y, b.y), p(z, a.z, b.z), p(w, a.w, b.w))
  def clamp(lower: Vec4d, upper: Vec4d): Vec4d = compwise(lower, upper)(Scalar.clamp)

  /** Convert to a String representation */
  override def toString = s"Vec4d$toPretty"

  def toPretty: String = "(%.2f, %.2f, %.2f, %.2f)".format(x, y, z, w)

  def toList: List[Double] = List(x, y, z, w)
  def toArray: Array[Double] = Array(x, y, z, w)

  def toVecNd: VecNd = VecNd(toArray)
  def toVecSd: VecSd = VecSd(toArray)

  def toVec4d: Vec4d = this
  def toVec3d: Vec3d = Vec3d(x, y, z)
  def toVec2d: Vec2d = Vec2d(x, y)
  def toVec1d: Vec1d = Vec1d(x)

  def toPos3d: Pos3d = Pos3d(x, y, z)
  def toPos2d: Pos2d = Pos2d(x, y)
  def toPos1d: Pos1d = Pos1d(x)

  def toIndex3i: Index3i = Index3i(x.toInt, y.toInt, z.toInt)
  def toIndex2i: Index2i = Index2i(x.toInt, y.toInt)
  def toIndex1i: Index1i = Index1i(x.toInt)

  def putNative(buf: CharBuffer) {
    buf.put(x.toChar); buf.put(y.toChar); buf.put(z.toChar); buf.put(w.toChar)
  }
  def >>>(buf: CharBuffer) { putNative(buf) }

  def putNative(buf: ShortBuffer) {
    buf.put(x.toShort); buf.put(y.toShort); buf.put(z.toShort); buf.put(w.toShort)
  }
  def >>>(buf: ShortBuffer) { putNative(buf) }

  def putNative(buf: IntBuffer) {
    buf.put(x.toInt); buf.put(y.toInt); buf.put(z.toInt); buf.put(w.toInt)
  }
  def >>>(buf: IntBuffer) { putNative(buf) }

  def putNative(buf: LongBuffer) {
    buf.put(x.toLong); buf.put(y.toLong); buf.put(z.toLong); buf.put(w.toLong)
  }
  def >>>(buf: LongBuffer) { putNative(buf) }

  def putNative(buf: FloatBuffer) {
    buf.put(x.toFloat); buf.put(y.toFloat); buf.put(z.toFloat); buf.put(w.toFloat)
  }
  def >>>(buf: FloatBuffer) { putNative(buf) }

  def putNative(buf: DoubleBuffer) {
    buf.put(x); buf.put(y); buf.put(z); buf.put(w)
  }
  def >>>(buf: DoubleBuffer) { putNative(buf) }
}

object Vec4d {
  def apply(s: Double): Vec4d = new Vec4d(s, s, s, s)
  def apply(x: Double, y: Double, z: Double, w: Double): Vec4d = new Vec4d(x, y, z, w)
  def apply(p: Vec1d, y: Double, z: Double, w: Double): Vec4d = new Vec4d(p.x, y, z, w)
  def apply(p: Vec2d, z: Double, w: Double): Vec4d = new Vec4d(p.x, p.y, z, w)
  def apply(p: Vec3d, w: Double): Vec4d = new Vec4d(p.x, p.y, p.z, w)

  def apply(ls: List[Double]): Vec4d =
    ls match {
      case List(x, y, z, w) => Vec4d(x, y, z, w)
      case List(x, y, z)    => Vec4d(x, y, z, 0.0)
      case List(x, y)       => Vec4d(x, y, 0.0, 0.0)
      case List(x)          => Vec4d(x, 0.0, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from List requires at least 1 and no more than 4 components!")
    }

  def apply(ary: Array[Double]): Vec4d =
    ary match {
      case Array(x, y, z, w) => Vec4d(x, y, z, w)
      case Array(x, y, z)    => Vec4d(x, y, z, 0.0)
      case Array(x, y)       => Vec4d(x, y, 0.0, 0.0)
      case Array(x)          => Vec4d(x, 0.0, 0.0, 0.0)
      case _ => throw new IllegalArgumentException(
        "Construct from Array requires at least 1 and no more than 4 components!")
    }

  def apply(buf: CharBuffer): Vec4d =
    Vec4d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: ShortBuffer): Vec4d =
    Vec4d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: IntBuffer): Vec4d =
    Vec4d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: LongBuffer): Vec4d =
    Vec4d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: FloatBuffer): Vec4d =
    Vec4d(buf.get.toDouble, buf.get.toDouble, buf.get.toDouble, buf.get.toDouble)
  def apply(buf: DoubleBuffer): Vec4d =
    Vec4d(buf.get, buf.get, buf.get, buf.get)

  def unapply(v: Vec4d): Some[(Double, Double, Double, Double)] = Some((v.x, v.y, v.z, v.w))

  def random(rand: Random = new Random): Vec4d =
    new Vec4d(rand.nextDouble, rand.nextDouble, rand.nextDouble, rand.nextDouble)

  @tailrec
  def randomUnit(rand: Random = new Random): Vec4d = {
    val v = random(rand) - Vec4d(0.5)
    val ms = v.magSq
    if ((ms < 0.25) && (ms > 0.001)) v / scala.math.sqrt(ms)
    else randomUnit(rand)
  }

  val unitX = Vec4d(1.0, 0.0, 0.0, 0.0)
  val unitY = Vec4d(0.0, 1.0, 0.0, 0.0)
  val unitZ = Vec4d(0.0, 0.0, 1.0, 0.0)
  val unitW = Vec4d(0.0, 0.0, 0.0, 1.0)
  val zero = Vec4d(0.0)
  val size = zero.size
}
