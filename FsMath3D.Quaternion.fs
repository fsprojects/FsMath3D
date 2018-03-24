(*
** F# 3D Math
** Copyright 2015-2018(c) Wael El Oraiby
** 
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
** 
** 1. Redistributions of source code must retain the above copyright notice,
**    this list of conditions and the following disclaimer.
** 
** 2. Redistributions in binary form must reproduce the above copyright notice,
**    this list of conditions and the following disclaimer in the documentation
**    and/or other materials provided with the distribution.
** 
** 3. Neither the name of the copyright holder nor the names of its contributors
**    may be used to endorse or promote products derived from this software
**    without specific prior written permission.
** 
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
** AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
** IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
** ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
** LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
** CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
** SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
** INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
** CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
** ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
** POSSIBILITY OF SUCH DAMAGE.
*)

module FsMath3D.Quaternion

open System
open FsMath3D.Vector
open FsMath3D.Matrix

type quat = struct
    val x   : single
    val y   : single
    val z   : single
    val w   : single
    
    new(x, y, z,  w)    = { x = x; y = y; z = z; w = w }

    static member (~-) (q: quat) = quat(-q.x, -q.y, -q.z, -q.w)
    static member (+) (q0: quat, q1: quat) = quat(q0.x + q1.x, q0.y + q1.y, q0.z + q1.z, q0.w + q1.w)
    static member (+) (q0: quat, q1: single) = quat(q0.x + q1, q0.y + q1, q0.z + q1, q0.w + q1)
    static member (-) (q0: quat, q1: quat) = quat(q0.x - q1.x, q0.y - q1.y, q0.z - q1.z, q0.w - q1.w)
    static member (-) (q0: quat, q1: single) = quat(q0.x - q1, q0.y - q1, q0.z - q1, q0.w - q1)
    static member (-) (q0: single, q1: quat) = quat(q0 - q1.x, q0 - q1.y, q0 - q1.z, q0 - q1.w)
    static member (*) (q0: quat, q1: single) = quat(q0.x * q1, q0.y * q1, q0.z * q1, q0.w * q1)
    static member (*) (q0: single, q1: quat) = quat(q0 * q1.x, q0 * q1.y, q0 * q1.z, q0 * q1.w)
    static member (/) (q0: quat, q1: single) = quat(q0.x / q1, q0.y / q1, q0.z / q1, q0.w / q1)
    static member (/) (q0: single, q1: quat) = quat(q0 / q1.x, q0 / q1.y, q0 / q1.z, q0 / q1.w)

    static member dot (q0: quat, q1: quat) = q0.x * q1.x + q0.y * q1.y + q0.z * q1.z + q0.w * q1.w
    static member length (q: quat)  = sqrt (q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w)
    static member conjugate (q: quat) = quat(-q.x, -q.y, -q.z, q.w)
    static member normalize (q: quat) = if quat.length q > 0.0f then q / quat.length(q) else q
    static member inverse (q: quat)   = quat.normalize <| quat.conjugate q

    static member (*) (q0: quat, q1: quat) =
        let x = q0.w * q1.x + q0.x * q1.w + q0.y * q1.z - q0.z * q1.y
        let y = q0.w * q1.y + q0.y * q1.w + q0.z * q1.x - q0.x * q1.z
        let z = q0.w * q1.z + q0.z * q1.w + q0.x * q1.y - q0.y * q1.x
        let w = q0.w * q1.w - q0.x * q1.x - q0.y * q1.y - q0.z * q1.z
        quat.normalize <| quat(x, y, z, w)

    static member toMat3 (q: quat) =
        let xx = q.x * q.x
        let xy = q.x * q.y
        let xz = q.x * q.z
        let xw = q.x * q.w
        let yy = q.y * q.y
        let yz = q.y * q.z
        let yw = q.y * q.w
        let zz = q.z * q.z
        let zw = q.z * q.w

        let m00 = 1.0f - 2.0f * (yy + zz)
        let m01 = 2.0f * (xy - zw)
        let m02 = 2.0f * (xz + yw)
        let m10 = 2.0f * (xy + zw)
        let m11 = 1.0f - 2.0f * (xx + zz)
        let m12 = 2.0f * (yz - xw)
        let m20 = 2.0f * (xz - yw)
        let m21 = 2.0f * (yz + xw)
        let m22 = 1.0f - 2.0f * (xx + yy)

        mat3 (m00, m10, m20, m01, m11, m21, m02, m12, m22)


    static member toMat4 (q: quat) =
        let xx = q.x * q.x
        let xy = q.x * q.y
        let xz = q.x * q.z
        let xw = q.x * q.w
        let yy = q.y * q.y
        let yz = q.y * q.z
        let yw = q.y * q.w
        let zz = q.z * q.z
        let zw = q.z * q.w

        let m00 = 1.0f - 2.0f * (yy + zz)
        let m01 = 2.0f * (xy - zw)
        let m02 = 2.0f * (xz + yw)
        let m10 = 2.0f * (xy + zw)
        let m11 = 1.0f - 2.0f * (xx + zz)
        let m12 = 2.0f * (yz - xw)
        let m20 = 2.0f * (xz - yw)
        let m21 = 2.0f * (yz + xw)
        let m22 = 1.0f - 2.0f * (xx + yy)

        mat4(m00, m10, m20, 0.0f,
             m01, m11, m21, 0.0f,
             m02, m12, m22, 0.0f,
             0.0f, 0.0f, 0.0f, 1.0f)
    
    static member toAxisAngle (q: quat) =
        let nq = quat.normalize q
        let cos_a = nq.w
        let sin_a = sqrt (1.0f - cos_a * cos_a)

        let angle = (acos cos_a) * 2.0f
        let sin_a = if( abs sin_a < (1.0f / 8192.0f) ) then 1.0f else sin_a
        let axis = vec3(nq.x, nq.y, nq.z) / sin_a

        axis, angle

    static member ofMat3 (m: mat3) =
        let mat0  = m.c0.x
        let mat1  = m.c1.x
        let mat2  = m.c2.x

        let mat4  = m.c0.y
        let mat5  = m.c1.y
        let mat6  = m.c2.y

        let mat8  = m.c0.z
        let mat9  = m.c1.z
        let mat10 = m.c2.z

        let t = 1.0f + mat0 + mat5 + mat10;

        let q =
            if( t > 0.0f )
            then
                let s = (sqrt t) * 2.0f

                let qx  = (mat9 - mat6) / s
                let qy  = (mat2 - mat8) / s
                let qz  = (mat4 - mat1) / s
                let qw  = 0.25f * s
                quat(qx, qy, qz, qw)
            else
                if( mat0 > mat5 && mat0 > mat10 )
                then
                    // Column 0:
                    let s = (sqrt (1.0f + mat0 - mat5 - mat10)) * 2.0f
                    let qx  = 0.25f * s
                    let qy  = (mat4 + mat1) / s
                    let qz  = (mat2 + mat8) / s
                    let qw  = (mat9 - mat6) / s
                    quat(qx, qy, qz, qw)
                elif( mat5 > mat10 )
                then
                    // Column 1:
                    let s = (sqrt (1.0f + mat5 - mat0 - mat10)) * 2.0f
                    let qx  = (mat4 + mat1) / s
                    let qy  = 0.25f * s
                    let qz  = (mat9 + mat6) / s
                    let qw  = (mat2 - mat8) / s
                    quat(qx, qy, qz, qw)
                else
                    // Column 2:
                    let s = (sqrt (1.0f + mat10 - mat0 - mat5)) * 2.0f
                    let qx  = (mat2 + mat8) / s
                    let qy  = (mat9 + mat6) / s
                    let qz  = 0.25f * s
                    let qw  = (mat4 - mat1) / s
                    quat(qx, qy, qz, qw)
        q

    static member ofMat4 (m: mat4) =
        let mat0  = m.c0.x
        let mat1  = m.c1.x
        let mat2  = m.c2.x

        let mat4  = m.c0.y
        let mat5  = m.c1.y
        let mat6  = m.c2.y

        let mat8  = m.c0.z
        let mat9  = m.c1.z
        let mat10 = m.c2.z

        let t = 1.0f + mat0 + mat5 + mat10

        let q =
            if( t > 0.0f )
            then
                let s = (sqrt t) * 2.0f

                let qx  = (mat9 - mat6) / s
                let qy  = (mat2 - mat8) / s
                let qz  = (mat4 - mat1) / s
                let qw  = 0.25f * s
                quat(qx, qy, qz, qw)
            else
                if( mat0 > mat5 && mat0 > mat10 )
                then
                    // Column 0:
                    let s = (sqrt (1.0f + mat0 - mat5 - mat10)) * 2.0f
                    let qx  = 0.25f * s
                    let qy  = (mat4 + mat1) / s
                    let qz  = (mat2 + mat8) / s
                    let qw  = (mat9 - mat6) / s
                    quat(qx, qy, qz, qw)
                elif( mat5 > mat10 )
                then
                    // Column 1:
                    let s = (sqrt (1.0f + mat5 - mat0 - mat10)) * 2.0f
                    let qx  = (mat4 + mat1) / s
                    let qy  = 0.25f * s
                    let qz  = (mat9 + mat6) / s
                    let qw  = (mat2 - mat8) / s
                    quat(qx, qy, qz, qw)
                else
                    // Column 2:
                    let s = (sqrt (1.0f + mat10 - mat0 - mat5)) * 2.0f
                    let qx  = (mat2 + mat8) / s
                    let qy  = (mat9 + mat6) / s
                    let qz  = 0.25f * s
                    let qw  = (mat4 - mat1) / s
                    quat(qx, qy, qz, qw)

        q

    static member ofAxisAngle (axis: vec3, angle: single) =
        let n = if vec3.length axis > 0.0f then vec3.normalize axis else axis
        let half_angle = angle * 0.5f
        let sin_a = sin half_angle
        let cos_a = cos half_angle

        let n = n * sin_a

        quat(n.x, n.y, n.z, cos_a)
    end

type dquat = struct
    val x   : double
    val y   : double
    val z   : double
    val w   : double
    
    new(x, y, z,  w)    = { x = x; y = y; z = z; w = w }

    static member (~-) (q: dquat) = dquat(-q.x, -q.y, -q.z, -q.w)
    static member (+) (q0: dquat, q1: dquat)  = dquat(q0.x + q1.x, q0.y + q1.y, q0.z + q1.z, q0.w + q1.w)
    static member (+) (q0: dquat, q1: double) = dquat(q0.x + q1, q0.y + q1, q0.z + q1, q0.w + q1)
    static member (-) (q0: dquat, q1: dquat)  = dquat(q0.x - q1.x, q0.y - q1.y, q0.z - q1.z, q0.w - q1.w)
    static member (-) (q0: dquat, q1: double) = dquat(q0.x - q1, q0.y - q1, q0.z - q1, q0.w - q1)
    static member (-) (q0: double, q1: dquat) = dquat(q0 - q1.x, q0 - q1.y, q0 - q1.z, q0 - q1.w)
    static member (*) (q0: dquat, q1: double) = dquat(q0.x * q1, q0.y * q1, q0.z * q1, q0.w * q1)
    static member (*) (q0: double, q1: dquat) = dquat(q0 * q1.x, q0 * q1.y, q0 * q1.z, q0 * q1.w)
    static member (/) (q0: dquat, q1: double) = dquat(q0.x / q1, q0.y / q1, q0.z / q1, q0.w / q1)
    static member (/) (q0: double, q1: dquat) = dquat(q0 / q1.x, q0 / q1.y, q0 / q1.z, q0 / q1.w)

    static member dot (q0: dquat, q1: dquat) = q0.x * q1.x + q0.y * q1.y + q0.z * q1.z + q0.w * q1.w
    static member length (q: dquat)  = sqrt (q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w)
    static member conjugate (q: dquat) = dquat(-q.x, -q.y, -q.z, q.w)
    static member normalize (q: dquat) = if dquat.length q > 0.0 then q / dquat.length(q) else q
    static member inverse (q: dquat)   = dquat.normalize <| dquat.conjugate q

    static member (*) (q0: dquat, q1: dquat) =
        let x = q0.w * q1.x + q0.x * q1.w + q0.y * q1.z - q0.z * q1.y
        let y = q0.w * q1.y + q0.y * q1.w + q0.z * q1.x - q0.x * q1.z
        let z = q0.w * q1.z + q0.z * q1.w + q0.x * q1.y - q0.y * q1.x
        let w = q0.w * q1.w - q0.x * q1.x - q0.y * q1.y - q0.z * q1.z
        dquat.normalize <| dquat(x, y, z, w)

    static member toMat3 (q: dquat) =
        let xx = q.x * q.x
        let xy = q.x * q.y
        let xz = q.x * q.z
        let xw = q.x * q.w
        let yy = q.y * q.y
        let yz = q.y * q.z
        let yw = q.y * q.w
        let zz = q.z * q.z
        let zw = q.z * q.w

        let m00 = 1.0 - 2.0 * (yy + zz)
        let m01 = 2.0 * (xy - zw)
        let m02 = 2.0 * (xz + yw)
        let m10 = 2.0 * (xy + zw)
        let m11 = 1.0 - 2.0 * (xx + zz)
        let m12 = 2.0 * (yz - xw)
        let m20 = 2.0 * (xz - yw)
        let m21 = 2.0 * (yz + xw)
        let m22 = 1.0 - 2.0 * (xx + yy)

        dmat3 (m00, m10, m20, m01, m11, m21, m02, m12, m22)


    static member toMat4 (q: dquat) =
        let xx = q.x * q.x
        let xy = q.x * q.y
        let xz = q.x * q.z
        let xw = q.x * q.w
        let yy = q.y * q.y
        let yz = q.y * q.z
        let yw = q.y * q.w
        let zz = q.z * q.z
        let zw = q.z * q.w

        let m00 = 1.0 - 2.0 * (yy + zz)
        let m01 = 2.0 * (xy - zw)
        let m02 = 2.0 * (xz + yw)
        let m10 = 2.0 * (xy + zw)
        let m11 = 1.0 - 2.0 * (xx + zz)
        let m12 = 2.0 * (yz - xw)
        let m20 = 2.0 * (xz - yw)
        let m21 = 2.0 * (yz + xw)
        let m22 = 1.0 - 2.0 * (xx + yy)

        dmat4(m00, m10, m20, 0.0,
              m01, m11, m21, 0.0,
              m02, m12, m22, 0.0,
              0.0, 0.0, 0.0, 1.0)
    
    static member toAxisAngle (q: dquat) =
        let nq = dquat.normalize q
        let cos_a = nq.w
        let sin_a = sqrt (1.0 - cos_a * cos_a)

        let angle = (acos cos_a) * 2.0
        let sin_a = if( abs sin_a < (1.0 / 8192.0) ) then 1.0 else sin_a
        let axis = dvec3(nq.x, nq.y, nq.z) / sin_a

        axis, angle

    static member ofMat3 (m: dmat3) =
        let mat0  = m.c0.x
        let mat1  = m.c1.x
        let mat2  = m.c2.x

        let mat4  = m.c0.y
        let mat5  = m.c1.y
        let mat6  = m.c2.y

        let mat8  = m.c0.z
        let mat9  = m.c1.z
        let mat10 = m.c2.z

        let t = 1.0 + mat0 + mat5 + mat10;

        let q =
            if( t > 0.0 )
            then
                let s = (sqrt t) * 2.0

                let qx  = (mat9 - mat6) / s
                let qy  = (mat2 - mat8) / s
                let qz  = (mat4 - mat1) / s
                let qw  = 0.25 * s
                dquat(qx, qy, qz, qw)
            else
                if( mat0 > mat5 && mat0 > mat10 )
                then
                    // Column 0:
                    let s = (sqrt (1.0 + mat0 - mat5 - mat10)) * 2.0
                    let qx  = 0.25 * s
                    let qy  = (mat4 + mat1) / s
                    let qz  = (mat2 + mat8) / s
                    let qw  = (mat9 - mat6) / s
                    dquat(qx, qy, qz, qw)
                elif( mat5 > mat10 )
                then
                    // Column 1:
                    let s = (sqrt (1.0 + mat5 - mat0 - mat10)) * 2.0
                    let qx  = (mat4 + mat1) / s
                    let qy  = 0.25 * s
                    let qz  = (mat9 + mat6) / s
                    let qw  = (mat2 - mat8) / s
                    dquat(qx, qy, qz, qw)
                else
                    // Column 2:
                    let s = (sqrt (1.0 + mat10 - mat0 - mat5)) * 2.0
                    let qx  = (mat2 + mat8) / s
                    let qy  = (mat9 + mat6) / s
                    let qz  = 0.25 * s
                    let qw  = (mat4 - mat1) / s
                    dquat(qx, qy, qz, qw)
        q

    static member ofMat4 (m: dmat4) =
        let mat0  = m.c0.x
        let mat1  = m.c1.x
        let mat2  = m.c2.x

        let mat4  = m.c0.y
        let mat5  = m.c1.y
        let mat6  = m.c2.y

        let mat8  = m.c0.z
        let mat9  = m.c1.z
        let mat10 = m.c2.z

        let t = 1.0 + mat0 + mat5 + mat10

        let q =
            if( t > 0.0 )
            then
                let s = (sqrt t) * 2.0

                let qx  = (mat9 - mat6) / s
                let qy  = (mat2 - mat8) / s
                let qz  = (mat4 - mat1) / s
                let qw  = 0.25 * s
                dquat(qx, qy, qz, qw)
            else
                if( mat0 > mat5 && mat0 > mat10 )
                then
                    // Column 0:
                    let s = (sqrt (1.0 + mat0 - mat5 - mat10)) * 2.0
                    let qx  = 0.25 * s
                    let qy  = (mat4 + mat1) / s
                    let qz  = (mat2 + mat8) / s
                    let qw  = (mat9 - mat6) / s
                    dquat(qx, qy, qz, qw)
                elif( mat5 > mat10 )
                then
                    // Column 1:
                    let s = (sqrt (1.0 + mat5 - mat0 - mat10)) * 2.0
                    let qx  = (mat4 + mat1) / s
                    let qy  = 0.25 * s
                    let qz  = (mat9 + mat6) / s
                    let qw  = (mat2 - mat8) / s
                    dquat(qx, qy, qz, qw)
                else
                    // Column 2:
                    let s = (sqrt (1.0 + mat10 - mat0 - mat5)) * 2.0
                    let qx  = (mat2 + mat8) / s
                    let qy  = (mat9 + mat6) / s
                    let qz  = 0.25 * s
                    let qw  = (mat4 - mat1) / s
                    dquat(qx, qy, qz, qw)

        q

    static member ofAxisAngle (axis: dvec3, angle: double) =
        let n = if dvec3.length axis > 0.0 then dvec3.normalize axis else axis
        let half_angle = angle * 0.5
        let sin_a = sin half_angle
        let cos_a = cos half_angle

        let n = n * sin_a

        dquat(n.x, n.y, n.z, cos_a)
    end