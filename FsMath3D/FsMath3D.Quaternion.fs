(*
** F# Rendering ToolKit
** Copyright (C) 2015  Wael El Oraiby
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
*)
module FsRTK.Math3D.Quaternion

open System
open FsRTK.Math3D.Vector
open FsRTK.Math3D.Matrix

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
    static member length (q: quat)  = Math.Sqrt ((q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w) |> float) |> single
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
        let sin_a = Math.Sqrt((1.0f - cos_a * cos_a) |> float) |> single

        let angle = (Math.Acos (cos_a |> float) |> single) * 2.0f
        let sin_a = if( Math.Abs(sin_a |> single) |> single < (1.0f / 8192.0f) ) then 1.0f else sin_a
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
                let s = (Math.Sqrt(t |> float) |> single) * 2.0f

                let qx  = (mat9 - mat6) / s
                let qy  = (mat2 - mat8) / s
                let qz  = (mat4 - mat1) / s
                let qw  = 0.25f * s
                quat(qx, qy, qz, qw)
            else
                if( mat0 > mat5 && mat0 > mat10 )
                then
                    // Column 0:
                    let s = (Math.Sqrt((1.0f + mat0 - mat5 - mat10) |> float) |> single) * 2.0f
                    let qx  = 0.25f * s
                    let qy  = (mat4 + mat1) / s
                    let qz  = (mat2 + mat8) / s
                    let qw  = (mat9 - mat6) / s
                    quat(qx, qy, qz, qw)
                elif( mat5 > mat10 )
                then
                    // Column 1:
                    let s = (Math.Sqrt((1.0f + mat5 - mat0 - mat10) |> float) |> single) * 2.0f
                    let qx  = (mat4 + mat1) / s
                    let qy  = 0.25f * s
                    let qz  = (mat9 + mat6) / s
                    let qw  = (mat2 - mat8) / s
                    quat(qx, qy, qz, qw)
                else
                    // Column 2:
                    let s = (Math.Sqrt((1.0f + mat10 - mat0 - mat5) |> float) |> single) * 2.0f
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
                let s = (Math.Sqrt(t |> float) |> single) * 2.0f

                let qx  = (mat9 - mat6) / s
                let qy  = (mat2 - mat8) / s
                let qz  = (mat4 - mat1) / s
                let qw  = 0.25f * s
                quat(qx, qy, qz, qw)
            else
                if( mat0 > mat5 && mat0 > mat10 )
                then
                    // Column 0:
                    let s = (Math.Sqrt((1.0f + mat0 - mat5 - mat10) |> float) |> single) * 2.0f
                    let qx  = 0.25f * s
                    let qy  = (mat4 + mat1) / s
                    let qz  = (mat2 + mat8) / s
                    let qw  = (mat9 - mat6) / s
                    quat(qx, qy, qz, qw)
                elif( mat5 > mat10 )
                then
                    // Column 1:
                    let s = (Math.Sqrt((1.0f + mat5 - mat0 - mat10) |> float) |> single) * 2.0f
                    let qx  = (mat4 + mat1) / s
                    let qy  = 0.25f * s
                    let qz  = (mat9 + mat6) / s
                    let qw  = (mat2 - mat8) / s
                    quat(qx, qy, qz, qw)
                else
                    // Column 2:
                    let s = (Math.Sqrt((1.0f + mat10 - mat0 - mat5) |> float) |> single) * 2.0f
                    let qx  = (mat2 + mat8) / s
                    let qy  = (mat9 + mat6) / s
                    let qz  = 0.25f * s
                    let qw  = (mat4 - mat1) / s
                    quat(qx, qy, qz, qw)

        q

    static member ofAxisAngle (axis: vec3, angle: single) =
        let n = if vec3.length axis > 0.0f then vec3.normalize axis else axis
        let half_angle = angle * 0.5f
        let sin_a = (Math.Sin (half_angle |> float)) |> single
        let cos_a = (Math.Cos (half_angle |> float)) |> single

        let n = n * sin_a

        quat(n.x, n.y, n.z, cos_a)
    end