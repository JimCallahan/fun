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

/** A 2-dimensional swizzling operations. */
trait Swizzle2[Repr2] {
  /** Swizzling operation. */
  def xx: Repr2

  /** Swizzling operation. */
  def yy: Repr2

  /** Swizzling operation. */
  def yx: Repr2
}

/** A 3-dimensional swizzling operations. */
trait Swizzle3[Repr2, Repr3]
    extends Swizzle2[Repr2] {

  /** Swizzling operation. */
  def xxx: Repr3

  /** Swizzling operation. */
  def xxy: Repr3

  /** Swizzling operation. */
  def xyx: Repr3

  /** Swizzling operation. */
  def yxx: Repr3

  /** Swizzling operation. */
  def xxz: Repr3

  /** Swizzling operation. */
  def xzx: Repr3

  /** Swizzling operation. */
  def zxx: Repr3

  /** Swizzling operation. */
  def xyy: Repr3

  /** Swizzling operation. */
  def yxy: Repr3

  /** Swizzling operation. */
  def yyx: Repr3

  /** Swizzling operation. */
  def xyz: Repr3

  /** Swizzling operation. */
  def xzy: Repr3

  /** Swizzling operation. */
  def yxz: Repr3

  /** Swizzling operation. */
  def yzx: Repr3

  /** Swizzling operation. */
  def zxy: Repr3

  /** Swizzling operation. */
  def zyx: Repr3

  /** Swizzling operation. */
  def xzz: Repr3

  /** Swizzling operation. */
  def zxz: Repr3

  /** Swizzling operation. */
  def zzx: Repr3

  /** Swizzling operation. */
  def yyy: Repr3

  /** Swizzling operation. */
  def yyz: Repr3

  /** Swizzling operation. */
  def yzy: Repr3

  /** Swizzling operation. */
  def zyy: Repr3

  /** Swizzling operation. */
  def yzz: Repr3

  /** Swizzling operation. */
  def zyz: Repr3

  /** Swizzling operation. */
  def zzy: Repr3

  /** Swizzling operation. */
  def zzz: Repr3
}

/** A 4-dimensional swizzling operations. */
trait Swizzle4[Repr2, Repr3, Repr4]
    extends Swizzle3[Repr2, Repr3] {

  /** Swizzling operation. */
  def xxxx: Repr4

  /** Swizzling operation. */
  def xxxy: Repr4

  /** Swizzling operation. */
  def xxyx: Repr4

  /** Swizzling operation. */
  def xyxx: Repr4

  /** Swizzling operation. */
  def yxxx: Repr4

  /** Swizzling operation. */
  def xxxz: Repr4

  /** Swizzling operation. */
  def xxzx: Repr4

  /** Swizzling operation. */
  def xzxx: Repr4

  /** Swizzling operation. */
  def zxxx: Repr4

  /** Swizzling operation. */
  def xxxw: Repr4

  /** Swizzling operation. */
  def xxwx: Repr4

  /** Swizzling operation. */
  def xwxx: Repr4

  /** Swizzling operation. */
  def wxxx: Repr4

  /** Swizzling operation. */
  def xxyy: Repr4

  /** Swizzling operation. */
  def xyxy: Repr4

  /** Swizzling operation. */
  def xyyx: Repr4

  /** Swizzling operation. */
  def yxxy: Repr4

  /** Swizzling operation. */
  def yxyx: Repr4

  /** Swizzling operation. */
  def yyxx: Repr4

  /** Swizzling operation. */
  def xxyz: Repr4

  /** Swizzling operation. */
  def xxzy: Repr4

  /** Swizzling operation. */
  def xyxz: Repr4

  /** Swizzling operation. */
  def xyzx: Repr4

  /** Swizzling operation. */
  def xzxy: Repr4

  /** Swizzling operation. */
  def xzyx: Repr4

  /** Swizzling operation. */
  def yxxz: Repr4

  /** Swizzling operation. */
  def yxzx: Repr4

  /** Swizzling operation. */
  def yzxx: Repr4

  /** Swizzling operation. */
  def zxxy: Repr4

  /** Swizzling operation. */
  def zxyx: Repr4

  /** Swizzling operation. */
  def zyxx: Repr4

  /** Swizzling operation. */
  def xxyw: Repr4

  /** Swizzling operation. */
  def xxwy: Repr4

  /** Swizzling operation. */
  def xyxw: Repr4

  /** Swizzling operation. */
  def xywx: Repr4

  /** Swizzling operation. */
  def xwxy: Repr4

  /** Swizzling operation. */
  def xwyx: Repr4

  /** Swizzling operation. */
  def yxxw: Repr4

  /** Swizzling operation. */
  def yxwx: Repr4

  /** Swizzling operation. */
  def ywxx: Repr4

  /** Swizzling operation. */
  def wxxy: Repr4

  /** Swizzling operation. */
  def wxyx: Repr4

  /** Swizzling operation. */
  def wyxx: Repr4

  /** Swizzling operation. */
  def xxzz: Repr4

  /** Swizzling operation. */
  def xzxz: Repr4

  /** Swizzling operation. */
  def xzzx: Repr4

  /** Swizzling operation. */
  def zxxz: Repr4

  /** Swizzling operation. */
  def zxzx: Repr4

  /** Swizzling operation. */
  def zzxx: Repr4

  /** Swizzling operation. */
  def xxzw: Repr4

  /** Swizzling operation. */
  def xxwz: Repr4

  /** Swizzling operation. */
  def xzxw: Repr4

  /** Swizzling operation. */
  def xzwx: Repr4

  /** Swizzling operation. */
  def xwxz: Repr4

  /** Swizzling operation. */
  def xwzx: Repr4

  /** Swizzling operation. */
  def zxxw: Repr4

  /** Swizzling operation. */
  def zxwx: Repr4

  /** Swizzling operation. */
  def zwxx: Repr4

  /** Swizzling operation. */
  def wxxz: Repr4

  /** Swizzling operation. */
  def wxzx: Repr4

  /** Swizzling operation. */
  def wzxx: Repr4

  /** Swizzling operation. */
  def xxww: Repr4

  /** Swizzling operation. */
  def xwxw: Repr4

  /** Swizzling operation. */
  def xwwx: Repr4

  /** Swizzling operation. */
  def wxxw: Repr4

  /** Swizzling operation. */
  def wxwx: Repr4

  /** Swizzling operation. */
  def wwxx: Repr4

  /** Swizzling operation. */
  def xyyy: Repr4

  /** Swizzling operation. */
  def yxyy: Repr4

  /** Swizzling operation. */
  def yyxy: Repr4

  /** Swizzling operation. */
  def yyyx: Repr4

  /** Swizzling operation. */
  def xyyz: Repr4

  /** Swizzling operation. */
  def xyzy: Repr4

  /** Swizzling operation. */
  def xzyy: Repr4

  /** Swizzling operation. */
  def yxyz: Repr4

  /** Swizzling operation. */
  def yxzy: Repr4

  /** Swizzling operation. */
  def yyxz: Repr4

  /** Swizzling operation. */
  def yyzx: Repr4

  /** Swizzling operation. */
  def yzxy: Repr4

  /** Swizzling operation. */
  def yzyx: Repr4

  /** Swizzling operation. */
  def zxyy: Repr4

  /** Swizzling operation. */
  def zyxy: Repr4

  /** Swizzling operation. */
  def zyyx: Repr4

  /** Swizzling operation. */
  def xyyw: Repr4

  /** Swizzling operation. */
  def xywy: Repr4

  /** Swizzling operation. */
  def xwyy: Repr4

  /** Swizzling operation. */
  def yxyw: Repr4

  /** Swizzling operation. */
  def yxwy: Repr4

  /** Swizzling operation. */
  def yyxw: Repr4

  /** Swizzling operation. */
  def yywx: Repr4

  /** Swizzling operation. */
  def ywxy: Repr4

  /** Swizzling operation. */
  def ywyx: Repr4

  /** Swizzling operation. */
  def wxyy: Repr4

  /** Swizzling operation. */
  def wyxy: Repr4

  /** Swizzling operation. */
  def wyyx: Repr4

  /** Swizzling operation. */
  def xyzz: Repr4

  /** Swizzling operation. */
  def xzyz: Repr4

  /** Swizzling operation. */
  def xzzy: Repr4

  /** Swizzling operation. */
  def yxzz: Repr4

  /** Swizzling operation. */
  def yzxz: Repr4

  /** Swizzling operation. */
  def yzzx: Repr4

  /** Swizzling operation. */
  def zxyz: Repr4

  /** Swizzling operation. */
  def zxzy: Repr4

  /** Swizzling operation. */
  def zyxz: Repr4

  /** Swizzling operation. */
  def zyzx: Repr4

  /** Swizzling operation. */
  def zzxy: Repr4

  /** Swizzling operation. */
  def zzyx: Repr4

  /** Swizzling operation. */
  def xyzw: Repr4

  /** Swizzling operation. */
  def xywz: Repr4

  /** Swizzling operation. */
  def xzyw: Repr4

  /** Swizzling operation. */
  def xzwy: Repr4

  /** Swizzling operation. */
  def xwyz: Repr4

  /** Swizzling operation. */
  def xwzy: Repr4

  /** Swizzling operation. */
  def yxzw: Repr4

  /** Swizzling operation. */
  def yxwz: Repr4

  /** Swizzling operation. */
  def yzxw: Repr4

  /** Swizzling operation. */
  def yzwx: Repr4

  /** Swizzling operation. */
  def ywxz: Repr4

  /** Swizzling operation. */
  def ywzx: Repr4

  /** Swizzling operation. */
  def zxyw: Repr4

  /** Swizzling operation. */
  def zxwy: Repr4

  /** Swizzling operation. */
  def zyxw: Repr4

  /** Swizzling operation. */
  def zywx: Repr4

  /** Swizzling operation. */
  def zwxy: Repr4

  /** Swizzling operation. */
  def zwyx: Repr4

  /** Swizzling operation. */
  def wxyz: Repr4

  /** Swizzling operation. */
  def wxzy: Repr4

  /** Swizzling operation. */
  def wyxz: Repr4

  /** Swizzling operation. */
  def wyzx: Repr4

  /** Swizzling operation. */
  def wzxy: Repr4

  /** Swizzling operation. */
  def wzyx: Repr4

  /** Swizzling operation. */
  def xyww: Repr4

  /** Swizzling operation. */
  def xwyw: Repr4

  /** Swizzling operation. */
  def xwwy: Repr4

  /** Swizzling operation. */
  def yxww: Repr4

  /** Swizzling operation. */
  def ywxw: Repr4

  /** Swizzling operation. */
  def ywwx: Repr4

  /** Swizzling operation. */
  def wxyw: Repr4

  /** Swizzling operation. */
  def wxwy: Repr4

  /** Swizzling operation. */
  def wyxw: Repr4

  /** Swizzling operation. */
  def wywx: Repr4

  /** Swizzling operation. */
  def wwxy: Repr4

  /** Swizzling operation. */
  def wwyx: Repr4

  /** Swizzling operation. */
  def xzzz: Repr4

  /** Swizzling operation. */
  def zxzz: Repr4

  /** Swizzling operation. */
  def zzxz: Repr4

  /** Swizzling operation. */
  def zzzx: Repr4

  /** Swizzling operation. */
  def xzzw: Repr4

  /** Swizzling operation. */
  def xzwz: Repr4

  /** Swizzling operation. */
  def xwzz: Repr4

  /** Swizzling operation. */
  def zxzw: Repr4

  /** Swizzling operation. */
  def zxwz: Repr4

  /** Swizzling operation. */
  def zzxw: Repr4

  /** Swizzling operation. */
  def zzwx: Repr4

  /** Swizzling operation. */
  def zwxz: Repr4

  /** Swizzling operation. */
  def zwzx: Repr4

  /** Swizzling operation. */
  def wxzz: Repr4

  /** Swizzling operation. */
  def wzxz: Repr4

  /** Swizzling operation. */
  def wzzx: Repr4

  /** Swizzling operation. */
  def xzww: Repr4

  /** Swizzling operation. */
  def xwzw: Repr4

  /** Swizzling operation. */
  def xwwz: Repr4

  /** Swizzling operation. */
  def zxww: Repr4

  /** Swizzling operation. */
  def zwxw: Repr4

  /** Swizzling operation. */
  def zwwx: Repr4

  /** Swizzling operation. */
  def wxzw: Repr4

  /** Swizzling operation. */
  def wxwz: Repr4

  /** Swizzling operation. */
  def wzxw: Repr4

  /** Swizzling operation. */
  def wzwx: Repr4

  /** Swizzling operation. */
  def wwxz: Repr4

  /** Swizzling operation. */
  def wwzx: Repr4

  /** Swizzling operation. */
  def xwww: Repr4

  /** Swizzling operation. */
  def wxww: Repr4

  /** Swizzling operation. */
  def wwxw: Repr4

  /** Swizzling operation. */
  def wwwx: Repr4

  /** Swizzling operation. */
  def yyyy: Repr4

  /** Swizzling operation. */
  def yyyz: Repr4

  /** Swizzling operation. */
  def yyzy: Repr4

  /** Swizzling operation. */
  def yzyy: Repr4

  /** Swizzling operation. */
  def zyyy: Repr4

  /** Swizzling operation. */
  def yyyw: Repr4

  /** Swizzling operation. */
  def yywy: Repr4

  /** Swizzling operation. */
  def ywyy: Repr4

  /** Swizzling operation. */
  def wyyy: Repr4

  /** Swizzling operation. */
  def yyzz: Repr4

  /** Swizzling operation. */
  def yzyz: Repr4

  /** Swizzling operation. */
  def yzzy: Repr4

  /** Swizzling operation. */
  def zyyz: Repr4

  /** Swizzling operation. */
  def zyzy: Repr4

  /** Swizzling operation. */
  def zzyy: Repr4

  /** Swizzling operation. */
  def yyzw: Repr4

  /** Swizzling operation. */
  def yywz: Repr4

  /** Swizzling operation. */
  def yzyw: Repr4

  /** Swizzling operation. */
  def yzwy: Repr4

  /** Swizzling operation. */
  def ywyz: Repr4

  /** Swizzling operation. */
  def ywzy: Repr4

  /** Swizzling operation. */
  def zyyw: Repr4

  /** Swizzling operation. */
  def zywy: Repr4

  /** Swizzling operation. */
  def zwyy: Repr4

  /** Swizzling operation. */
  def wyyz: Repr4

  /** Swizzling operation. */
  def wyzy: Repr4

  /** Swizzling operation. */
  def wzyy: Repr4

  /** Swizzling operation. */
  def yyww: Repr4

  /** Swizzling operation. */
  def ywyw: Repr4

  /** Swizzling operation. */
  def ywwy: Repr4

  /** Swizzling operation. */
  def wyyw: Repr4

  /** Swizzling operation. */
  def wywy: Repr4

  /** Swizzling operation. */
  def wwyy: Repr4

  /** Swizzling operation. */
  def yzzz: Repr4

  /** Swizzling operation. */
  def zyzz: Repr4

  /** Swizzling operation. */
  def zzyz: Repr4

  /** Swizzling operation. */
  def zzzy: Repr4

  /** Swizzling operation. */
  def yzzw: Repr4

  /** Swizzling operation. */
  def yzwz: Repr4

  /** Swizzling operation. */
  def ywzz: Repr4

  /** Swizzling operation. */
  def zyzw: Repr4

  /** Swizzling operation. */
  def zywz: Repr4

  /** Swizzling operation. */
  def zzyw: Repr4

  /** Swizzling operation. */
  def zzwy: Repr4

  /** Swizzling operation. */
  def zwyz: Repr4

  /** Swizzling operation. */
  def zwzy: Repr4

  /** Swizzling operation. */
  def wyzz: Repr4

  /** Swizzling operation. */
  def wzyz: Repr4

  /** Swizzling operation. */
  def wzzy: Repr4

  /** Swizzling operation. */
  def yzww: Repr4

  /** Swizzling operation. */
  def ywzw: Repr4

  /** Swizzling operation. */
  def ywwz: Repr4

  /** Swizzling operation. */
  def zyww: Repr4

  /** Swizzling operation. */
  def zwyw: Repr4

  /** Swizzling operation. */
  def zwwy: Repr4

  /** Swizzling operation. */
  def wyzw: Repr4

  /** Swizzling operation. */
  def wywz: Repr4

  /** Swizzling operation. */
  def wzyw: Repr4

  /** Swizzling operation. */
  def wzwy: Repr4

  /** Swizzling operation. */
  def wwyz: Repr4

  /** Swizzling operation. */
  def wwzy: Repr4

  /** Swizzling operation. */
  def ywww: Repr4

  /** Swizzling operation. */
  def wyww: Repr4

  /** Swizzling operation. */
  def wwyw: Repr4

  /** Swizzling operation. */
  def wwwy: Repr4

  /** Swizzling operation. */
  def zzzz: Repr4

  /** Swizzling operation. */
  def zzzw: Repr4

  /** Swizzling operation. */
  def zzwz: Repr4

  /** Swizzling operation. */
  def zwzz: Repr4

  /** Swizzling operation. */
  def wzzz: Repr4

  /** Swizzling operation. */
  def zzww: Repr4

  /** Swizzling operation. */
  def zwzw: Repr4

  /** Swizzling operation. */
  def zwwz: Repr4

  /** Swizzling operation. */
  def wzzw: Repr4

  /** Swizzling operation. */
  def wzwz: Repr4

  /** Swizzling operation. */
  def wwzz: Repr4

  /** Swizzling operation. */
  def zwww: Repr4

  /** Swizzling operation. */
  def wzww: Repr4

  /** Swizzling operation. */
  def wwzw: Repr4

  /** Swizzling operation. */
  def wwwz: Repr4

  /** Swizzling operation. */
  def wwww: Repr4
}
