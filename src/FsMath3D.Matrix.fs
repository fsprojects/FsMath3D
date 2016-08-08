(*
** F# 3D Math
** Copyright (C) 2015-2016  Wael El Oraiby
** 
** This program is free software: you can redistribute it and/or modify
** it under the terms of the GNU Affero General Public License as
** published by the Free Software Foundation, either version 3 of the
** License, or (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Affero General Public License for more details.
** 
** You should have received a copy of the GNU Affero General Public License
** along with this program.  If not, see <http://www.gnu.org/licenses/>.
**
** Under Section 7 of GPL version 3, you are granted additional permissions
** described in the GCC Runtime Library Exception, version 3.1, as
** published by the Free Software Foundation.
** See <https://www.gnu.org/licenses/gcc-exception-3.1.en.html>
*)

module FsMath3D.Matrix

open System
open System.Runtime
open System.Runtime.InteropServices

open FsMath3D.Vector

(*
** matX structure
** The matrix is layed out as the following:
**        | m0 mN   ... |   | col[0].x col[1].x ... |
**   M =  | m1 mN+1 ... | = | col[0].y col[1].y ... |
**        | .. ..   ... |   | ...      ...      ... |
*)

type mat2 = struct
    val         c0  : vec2
    val         c1  : vec2

    new(c0, c1)         = { c0 = c0; c1 = c1 }

    new(m0, m1, m2, m3) = { c0 = vec2(m0, m1)
                            c1 = vec2(m2, m3) }

    member x.Item i =
        match i with
        | 0 -> x.c0
        | 1 -> x.c1
        | _ -> failwith "mat2: index out of range"

    member x.r0   = vec2(x.c0.x, x.c1.x)
    member x.r1   = vec2(x.c0.y, x.c1.y)

    static member inline identity = mat2(1.0f, 0.0f, 0.0f, 1.0f)

    static member inline transpose (m: mat2)   =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m01 = m.c1.x
        let m11 = m.c1.y
        mat2(m00, m01, m10, m11)

    static member inline (+) (a: mat2, b: mat2)      = mat2(a.c0 + b.c0, a.c1 + b.c1)
    static member inline (-) (a: mat2, b: mat2)      = mat2(a.c0 - b.c0, a.c1 - b.c1)

    static member inline (*) (a: mat2, b: single)    = mat2(a.c0 * b, a.c1 * b)
    static member inline (/) (a: mat2, b: single)    = mat2(a.c0 / b, a.c1 / b)

    static member inline (*) (a: mat2, b: mat2)      =
        let a00 = a.c0.x
        let a10 = a.c0.y
        let a01 = a.c1.x
        let a11 = a.c1.y

        let b00 = b.c0.x
        let b10 = b.c0.y
        let b01 = b.c1.x
        let b11 = b.c1.y

        let c00 = a00 * b00 + a01 * b10
        let c01 = a00 * b01 + a01 * b11
        let c10 = a10 * b00 + a11 * b10
        let c11 = a10 * b01 + a11 * b11

        mat2(c00, c10, c01, c11)

    static member inline determinant (m: mat2) =
        let m00 = m.c0.x
        let m10 = m.c0.y
 
        let m01 = m.c1.x
        let m11 = m.c1.y

        m00 * m11 - m01 * m10

    static member inline inverse (m: mat2) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m01 = m.c1.x
        let m11 = m.c1.y

        let inv_det = 1.0f / (m00 * m11 - m01 * m10)

        let r00 =  m11 * inv_det
        let r01 = -m01 * inv_det
        let r10 = -m10 * inv_det
        let r11 =  m00 * inv_det

        mat2(r00, r10, r01, r11)

    static member inline (/) (a: mat2, b: mat2)    = a * (mat2.inverse b)

    static member inline (*) (v: vec2, m: mat2)    = vec2(vec2.dot(m.c0, v), vec2.dot(m.c1, v))
    static member inline (*) (m: mat2, v: vec2)    = vec2(vec2.dot(m.r0, v), vec2.dot(m.r1, v))
    
    end

type mat3 = struct
    val         c0  : vec3
    val         c1  : vec3
    val         c2  : vec3

    new(c0, c1, c2)     = { c0 = c0; c1 = c1; c2 = c2 }

    new(m0, m1, m2,
        m3, m4, m5,
        m6, m7, m8) = { c0 = vec3(m0, m1, m2)
                        c1 = vec3(m3, m4, m5)
                        c2 = vec3(m6, m7, m8) }

    member x.Item i =
        match i with
        | 0 -> x.c0
        | 1 -> x.c1
        | 2 -> x.c2
        | _ -> failwith "mat3: index out of range"

    member x.r0   = vec3(x.c0.x, x.c1.x, x.c2.x)
    member x.r1   = vec3(x.c0.y, x.c1.y, x.c2.y)
    member x.r2   = vec3(x.c0.z, x.c1.z, x.c2.z)

    static member inline identity = mat3(1.0f, 0.0f, 0.0f,
                                         0.0f, 1.0f, 0.0f,
                                         0.0f, 0.0f, 1.0f)

    static member inline transpose (m: mat3) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m20 = m.c0.z
        
        let m01 = m.c1.x
        let m11 = m.c1.y
        let m21 = m.c1.z
        
        let m02 = m.c2.x
        let m12 = m.c2.y
        let m22 = m.c2.z

        mat3(m00, m01, m02,
             m10, m11, m12,
             m20, m21, m22)

    static member inline (+) (a: mat3, b: mat3)      = mat3(a.c0 + b.c0, a.c1 + b.c1, a.c2 + b.c2)
    static member inline (-) (a: mat3, b: mat3)      = mat3(a.c0 - b.c0, a.c1 - b.c1, a.c2 - b.c2)

    static member inline (*) (a: mat3, b: single)    = mat3(a.c0 * b, a.c1 * b, a.c2 * b)
    static member inline (/) (a: mat3, b: single)    = mat3(a.c0 / b, a.c1 / b, a.c2 / b)

    static member inline (*) (a: mat3, b: mat3)      =
        let a00 = a.c0.x
        let a10 = a.c0.y
        let a20 = a.c0.z
        
        let a01 = a.c1.x
        let a11 = a.c1.y
        let a21 = a.c1.z
        
        let a02 = a.c2.x
        let a12 = a.c2.y
        let a22 = a.c2.z
        
        let b00 = b.c0.x
        let b10 = b.c0.y
        let b20 = b.c0.z
        
        let b01 = b.c1.x
        let b11 = b.c1.y
        let b21 = b.c1.z
        
        let b02 = b.c2.x
        let b12 = b.c2.y
        let b22 = b.c2.z
        
        let c00 = a00 * b00 + a01 * b10 + a02 * b20
        let c01 = a00 * b01 + a01 * b11 + a02 * b21
        let c02 = a00 * b02 + a01 * b12 + a02 * b22
        
        let c10 = a10 * b00 + a11 * b10 + a12 * b20
        let c11 = a10 * b01 + a11 * b11 + a12 * b21
        let c12 = a10 * b02 + a11 * b12 + a12 * b22
        
        let c20 = a20 * b00 + a21 * b10 + a22 * b20
        let c21 = a20 * b01 + a21 * b11 + a22 * b21
        let c22 = a20 * b02 + a21 * b12 + a22 * b22

        mat3(c00, c10, c20,
             c01, c11, c21,
             c02, c12, c22)

    static member inline determinant (m: mat3) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m20 = m.c0.z
        
        let m01 = m.c1.x
        let m11 = m.c1.y
        let m21 = m.c1.z
        
        let m02 = m.c2.x
        let m12 = m.c2.y
        let m22 = m.c2.z
        
        m00 * m11 * m22 +
        m01 * m12 * m20 +
        m02 * m10 * m21 -
        m00 * m12 * m21 -
        m01 * m10 * m22 -
        m02 * m11 * m20

    static member inline inverse (m: mat3) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m20 = m.c0.z
        
        let m01 = m.c1.x
        let m11 = m.c1.y
        let m21 = m.c1.z
        
        let m02 = m.c2.x
        let m12 = m.c2.y
        let m22 = m.c2.z
        
        let inv_det = 1.0f / (m00 * m11 * m22 +
                              m01 * m12 * m20 +
                              m02 * m10 * m21 -
                              m00 * m12 * m21 -
                              m01 * m10 * m22 -
                              m02 * m11 * m20)
        
        let r00 = (m11 * m22 - m12 * m21) * inv_det
        let r01 = (m02 * m21 - m01 * m22) * inv_det
        let r02 = (m01 * m12 - m02 * m11) * inv_det
        let r10 = (m12 * m20 - m10 * m22) * inv_det
        let r11 = (m00 * m22 - m02 * m20) * inv_det
        let r12 = (m02 * m10 - m00 * m12) * inv_det
        let r20 = (m10 * m21 - m11 * m20) * inv_det
        let r21 = (m01 * m20 - m00 * m21) * inv_det
        let r22 = (m00 * m11 - m01 * m10) * inv_det

        mat3(r00, r10, r20, r01, r11, r21, r02, r12, r22)
            
    static member inline (/) (a: mat3, b: mat3)    = a * (mat3.inverse b)

    static member inline (*) (v: vec3, m: mat3)    = vec3(vec3.dot(m.c0, v), vec3.dot(m.c1, v), vec3.dot(m.c2, v))
    static member inline (*) (m: mat3, v: vec3)    = vec3(vec3.dot(m.r0, v), vec3.dot(m.r1, v), vec3.dot(m.r2, v))

    end

type mat4 = struct
    val         c0  : vec4
    val         c1  : vec4
    val         c2  : vec4
    val         c3  : vec4

    new(c0, c1, c2, c3) = { c0 = c0; c1 = c1; c2 = c2; c3 = c3 }

    new(m0,  m1,  m2,  m3,
        m4,  m5,  m6,  m7,
        m8,  m9,  m10, m11,
        m12, m13, m14, m15) = { c0 = vec4(m0,  m1,  m2,  m3)
                                c1 = vec4(m4,  m5,  m6,  m7)
                                c2 = vec4(m8,  m9,  m10, m11)
                                c3 = vec4(m12, m13, m14, m15) }
            
    member x.toArray() =
        let c0 = x.c0.toArray()
        let c1 = x.c1.toArray()
        let c2 = x.c2.toArray()
        let c3 = x.c3.toArray()

        [| yield! c0
           yield! c1
           yield! c2
           yield! c3 |] 
                              
    member x.Item i =
        match i with
        | 0 -> x.c0
        | 1 -> x.c1
        | 2 -> x.c2
        | 3 -> x.c3
        | _ -> failwith "mat4: index out of range"

    member x.r0   = vec4(x.c0.x, x.c1.x, x.c2.x, x.c3.x)
    member x.r1   = vec4(x.c0.y, x.c1.y, x.c2.y, x.c3.y)
    member x.r2   = vec4(x.c0.z, x.c1.z, x.c2.z, x.c3.z)
    member x.r3   = vec4(x.c0.w, x.c1.w, x.c2.w, x.c3.w)

    static member inline identity = mat4(1.0f, 0.0f, 0.0f, 0.0f,
                                         0.0f, 1.0f, 0.0f, 0.0f,
                                         0.0f, 0.0f, 1.0f, 0.0f,
                                         0.0f, 0.0f, 0.0f, 1.0f)

    static member inline transpose (m: mat4) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m20 = m.c0.z
        let m30 = m.c0.w

        let m01 = m.c1.x
        let m11 = m.c1.y
        let m21 = m.c1.z
        let m31 = m.c1.w

        let m02 = m.c2.x
        let m12 = m.c2.y
        let m22 = m.c2.z
        let m32 = m.c2.w

        let m03 = m.c3.x
        let m13 = m.c3.y
        let m23 = m.c3.z
        let m33 = m.c3.w

        mat4(m00, m01, m02, m03,
             m10, m11, m12, m13,
             m20, m21, m22, m23,
             m30, m31, m32, m33)

    static member inline (+) (a: mat4, b: mat4)      = mat4(a.c0 + b.c0, a.c1 + b.c1, a.c2 + b.c2, a.c3 + b.c3)
    static member inline (-) (a: mat4, b: mat4)      = mat4(a.c0 - b.c0, a.c1 - b.c1, a.c2 - b.c2, a.c3 - b.c3)

    static member inline (*) (a: mat4, b: single)    = mat4(a.c0 * b, a.c1 * b, a.c2 * b, a.c3 * b)
    static member inline (/) (a: mat4, b: single)    = mat4(a.c0 / b, a.c1 / b, a.c2 / b, a.c3 / b)

    static member inline (*) (a: mat4, b: mat4) =
        let a00 = a.c0.x
        let a10 = a.c0.y
        let a20 = a.c0.z
        let a30 = a.c0.w

        let a01 = a.c1.x
        let a11 = a.c1.y
        let a21 = a.c1.z
        let a31 = a.c1.w

        let a02 = a.c2.x
        let a12 = a.c2.y
        let a22 = a.c2.z
        let a32 = a.c2.w

        let a03 = a.c3.x
        let a13 = a.c3.y
        let a23 = a.c3.z
        let a33 = a.c3.w

        let b00 = b.c0.x
        let b10 = b.c0.y
        let b20 = b.c0.z
        let b30 = b.c0.w

        let b01 = b.c1.x
        let b11 = b.c1.y
        let b21 = b.c1.z
        let b31 = b.c1.w

        let b02 = b.c2.x
        let b12 = b.c2.y
        let b22 = b.c2.z
        let b32 = b.c2.w

        let b03 = b.c3.x
        let b13 = b.c3.y
        let b23 = b.c3.z
        let b33 = b.c3.w

        let c00 = a00 * b00 + a01 * b10 + a02 * b20 + a03 * b30
        let c01 = a00 * b01 + a01 * b11 + a02 * b21 + a03 * b31
        let c02 = a00 * b02 + a01 * b12 + a02 * b22 + a03 * b32
        let c03 = a00 * b03 + a01 * b13 + a02 * b23 + a03 * b33

        let c10 = a10 * b00 + a11 * b10 + a12 * b20 + a13 * b30
        let c11 = a10 * b01 + a11 * b11 + a12 * b21 + a13 * b31
        let c12 = a10 * b02 + a11 * b12 + a12 * b22 + a13 * b32
        let c13 = a10 * b03 + a11 * b13 + a12 * b23 + a13 * b33

        let c20 = a20 * b00 + a21 * b10 + a22 * b20 + a23 * b30
        let c21 = a20 * b01 + a21 * b11 + a22 * b21 + a23 * b31
        let c22 = a20 * b02 + a21 * b12 + a22 * b22 + a23 * b32
        let c23 = a20 * b03 + a21 * b13 + a22 * b23 + a23 * b33

        let c30 = a30 * b00 + a31 * b10 + a32 * b20 + a33 * b30
        let c31 = a30 * b01 + a31 * b11 + a32 * b21 + a33 * b31
        let c32 = a30 * b02 + a31 * b12 + a32 * b22 + a33 * b32
        let c33 = a30 * b03 + a31 * b13 + a32 * b23 + a33 * b33

        mat4(c00, c10, c20, c30,
             c01, c11, c21, c31,
             c02, c12, c22, c32,
             c03, c13, c23, c33)

    static member inline determinant (m: mat4) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m20 = m.c0.z
        let m30 = m.c0.w

        let m01 = m.c1.x
        let m11 = m.c1.y
        let m21 = m.c1.z
        let m31 = m.c1.w

        let m02 = m.c2.x
        let m12 = m.c2.y
        let m22 = m.c2.z
        let m32 = m.c2.w

        let m03 = m.c3.x
        let m13 = m.c3.y
        let m23 = m.c3.z
        let m33 = m.c3.w

        m03 * m12 * m21 * m30 - m02 * m13 * m21 * m30 -
        m03 * m11 * m22 * m30 + m01 * m13 * m22 * m30 +
        m02 * m11 * m23 * m30 - m01 * m12 * m23 * m30 -
        m03 * m12 * m20 * m31 + m02 * m13 * m20 * m31 +
        m03 * m10 * m22 * m31 - m00 * m13 * m22 * m31 -
        m02 * m10 * m23 * m31 + m00 * m12 * m23 * m31 +
        m03 * m11 * m20 * m32 - m01 * m13 * m20 * m32 -
        m03 * m10 * m21 * m32 + m00 * m13 * m21 * m32 +
        m01 * m10 * m23 * m32 - m00 * m11 * m23 * m32 -
        m02 * m11 * m20 * m33 + m01 * m12 * m20 * m33 +
        m02 * m10 * m21 * m33 - m00 * m12 * m21 * m33 -
        m01 * m10 * m22 * m33 + m00 * m11 * m22 * m33
        
    static member inline inverse (m: mat4) =
        let m00 = m.c0.x
        let m10 = m.c0.y
        let m20 = m.c0.z
        let m30 = m.c0.w

        let m01 = m.c1.x
        let m11 = m.c1.y
        let m21 = m.c1.z
        let m31 = m.c1.w

        let m02 = m.c2.x
        let m12 = m.c2.y
        let m22 = m.c2.z
        let m32 = m.c2.w

        let m03 = m.c3.x
        let m13 = m.c3.y
        let m23 = m.c3.z
        let m33 = m.c3.w

        let denom   = m03 * m12 * m21 * m30 - m02 * m13 * m21 * m30 -
                      m03 * m11 * m22 * m30 + m01 * m13 * m22 * m30 +
                      m02 * m11 * m23 * m30 - m01 * m12 * m23 * m30 -
                      m03 * m12 * m20 * m31 + m02 * m13 * m20 * m31 +
                      m03 * m10 * m22 * m31 - m00 * m13 * m22 * m31 -
                      m02 * m10 * m23 * m31 + m00 * m12 * m23 * m31 +
                      m03 * m11 * m20 * m32 - m01 * m13 * m20 * m32 -
                      m03 * m10 * m21 * m32 + m00 * m13 * m21 * m32 +
                      m01 * m10 * m23 * m32 - m00 * m11 * m23 * m32 -
                      m02 * m11 * m20 * m33 + m01 * m12 * m20 * m33 +
                      m02 * m10 * m21 * m33 - m00 * m12 * m21 * m33 -
                      m01 * m10 * m22 * m33 + m00 * m11 * m22 * m33

        let inv_det = 1.0f / denom
        
        let r00 = (m12 * m23 * m31 - m13 * m22 * m31 +
                   m13 * m21 * m32 - m11 * m23 * m32 -
                   m12 * m21 * m33 + m11 * m22 * m33) * inv_det
        
        let r01 = (m03 * m22 * m31 - m02 * m23 * m31 -
                   m03 * m21 * m32 + m01 * m23 * m32 +
                   m02 * m21 * m33 - m01 * m22 * m33) * inv_det
        
        let r02 = (m02 * m13 * m31 - m03 * m12 * m31 +
                   m03 * m11 * m32 - m01 * m13 * m32 -
                   m02 * m11 * m33 + m01 * m12 * m33) * inv_det
        
        let r03 = (m03 * m12 * m21 - m02 * m13 * m21 -
                   m03 * m11 * m22 + m01 * m13 * m22 +
                   m02 * m11 * m23 - m01 * m12 * m23) * inv_det

        let r10 = (m13 * m22 * m30 - m12 * m23 * m30 -
                   m13 * m20 * m32 + m10 * m23 * m32 +
                   m12 * m20 * m33 - m10 * m22 * m33) * inv_det

        let r11 = (m02 * m23 * m30 - m03 * m22 * m30 +
                   m03 * m20 * m32 - m00 * m23 * m32 -
                   m02 * m20 * m33 + m00 * m22 * m33) * inv_det

        let r12 = (m03 * m12 * m30 - m02 * m13 * m30 -
                   m03 * m10 * m32 + m00 * m13 * m32 +
                   m02 * m10 * m33 - m00 * m12 * m33) * inv_det

        let r13 = (m02 * m13 * m20 - m03 * m12 * m20 +
                   m03 * m10 * m22 - m00 * m13 * m22 -
                   m02 * m10 * m23 + m00 * m12 * m23) * inv_det

        let r20 = (m11 * m23 * m30 - m13 * m21 * m30 +
                   m13 * m20 * m31 - m10 * m23 * m31 -
                   m11 * m20 * m33 + m10 * m21 * m33) * inv_det
        
        let r21 = (m03 * m21 * m30 - m01 * m23 * m30 -
                   m03 * m20 * m31 + m00 * m23 * m31 +
                   m01 * m20 * m33 - m00 * m21 * m33) * inv_det
        
        let r22 = (m01 * m13 * m30 - m03 * m11 * m30 +
                   m03 * m10 * m31 - m00 * m13 * m31 -
                   m01 * m10 * m33 + m00 * m11 * m33) * inv_det
                   
        let r23 = (m03 * m11 * m20 - m01 * m13 * m20 -
                   m03 * m10 * m21 + m00 * m13 * m21 +
                   m01 * m10 * m23 - m00 * m11 * m23) * inv_det
                   
        let r30 = (m12 * m21 * m30 - m11 * m22 * m30 -
                   m12 * m20 * m31 + m10 * m22 * m31 +
                   m11 * m20 * m32 - m10 * m21 * m32) * inv_det
                   
        let r31 = (m01 * m22 * m30 - m02 * m21 * m30 +
                   m02 * m20 * m31 - m00 * m22 * m31 -
                   m01 * m20 * m32 + m00 * m21 * m32) * inv_det
                   
        let r32 = (m02 * m11 * m30 - m01 * m12 * m30 -
                   m02 * m10 * m31 + m00 * m12 * m31 +
                   m01 * m10 * m32 - m00 * m11 * m32) * inv_det
                   
        let r33 = (m01 * m12 * m20 - m02 * m11 * m20 +
                   m02 * m10 * m21 - m00 * m12 * m21 -
                   m01 * m10 * m22 + m00 * m11 * m22) * inv_det
            
        mat4(r00, r10, r20, r30,
             r01, r11, r21, r31,
             r02, r12, r22, r32,
             r03, r13, r23, r33)

    static member inline (/) (a: mat4, b: mat4)    = a * (mat4.inverse b)

    static member inline (*) (v: vec4, m: mat4)    = vec4(vec4.dot(m.c0, v), vec4.dot(m.c1, v), vec4.dot(m.c2, v), vec4.dot(m.c3, v))
    static member inline (*) (m: mat4, v: vec4)    = vec4(vec4.dot(m.r0, v), vec4.dot(m.r1, v), vec4.dot(m.r2, v), vec4.dot(m.r3, v))

    end