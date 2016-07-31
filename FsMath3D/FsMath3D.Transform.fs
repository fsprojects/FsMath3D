(*
** F# 3D Math
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

module FsMath3D.Transform

open FsMath3D.Vector
open FsMath3D.Matrix
open FsMath3D.Quaternion

////////////////////////////////////////////////////////////////////////////////
/// orthogonal 2d projection matrix (both tested)
////////////////////////////////////////////////////////////////////////////////
type mat4
with
    static member ortho(left, right, bottom, top, near, far) =
        let width   = right - left
        let height  = top - bottom
        let depth   = far - near
        let r00     = 2.0f / width
        let r11     = 2.0f / height
        let r22     = -2.0f / depth
        let r03     = -(right + left) / width
        let r13     = -(top + bottom) / height
        let r23     = -(far + near) / depth
        mat4( r00 , 0.0f, 0.0f, 0.0f
            , 0.0f, r11,  0.0f, 0.0f
            , 0.0f, 0.0f, r22,  0.0f
            , r03,  r13,  r23,  1.0f)

    static member ortho(leftBottom: vec2, rightTop: vec2) = mat4.ortho(leftBottom.x, rightTop.x, leftBottom.y, rightTop.y, -1.0f, 1.0f)
    static member ortho(left, right, bottom, top) = mat4.ortho(left, right, bottom, top, -1.0f, 1.0f)
    static member ortho(lbn: vec3, rtf: vec3) = mat4.ortho(lbn.x, rtf.x, lbn.y, rtf.y, lbn.z, rtf.z)


    /// translation matrix (tested)
    static member translation(trans: vec3) =
        mat4( 1.0f, 0.0f, 0.0f, 0.0f
            , 0.0f, 1.0f, 0.0f, 0.0f
            , 0.0f, 0.0f, 1.0f, 0.0f
            , trans.x, trans.y, trans.z, 1.0f)

    /// scale matrix (tested)
    static member scale(scale: vec3) =
        mat4( scale.x, 0.0f, 0.0f, 0.0f
            , 0.0f, scale.y, 0.0f, 0.0f
            , 0.0f, 0.0f, scale.z, 0.0f
            , 0.0f, 0.0f, 0.0f, 1.0f)


    /// rotation matrix
    /// @{
    static member rotation(q: quat) = quat.toMat4 q
    static member rotation(angle: single, axis: vec3) = quat.ofAxisAngle (axis, angle) |> quat.toMat4

    /// project a point to the screen (tested)
    static member project(world: mat4, persp: mat4, lb: vec2, rt: vec2, pt: vec3) =
        let inp = vec4(pt.x, pt.y, pt.z, 1.0f)
        let pw  = persp * world
        let out = pw * inp

        let out = out / out.w

        let ox  = lb.x + ((rt.x - lb.x) * (out.x + 1.0f) * 0.5f)
        let oy  = lb.y + ((rt.y - lb.y) * (out.y + 1.0f) * 0.5f)
        let oz  = (out.z + 1.0f) * 0.5f

        vec3(ox, oy, oz)

    /// unproject a point to the 3d system (tested)
    static member unproject(world: mat4, persp: mat4, lb: vec2, rt: vec2, pt: vec3) =
        let pw  = persp * world
        let inv = mat4.inverse pw
        let inx = (2.0f * (pt.x - lb.x) / (rt.x - lb.x)) - 1.0f
        let iny = (2.0f * (pt.y - lb.y) / (rt.y - lb.y)) - 1.0f
        let inz = (2.0f * pt.z) - 1.0f
        let inw = 1.0f
        let inp = vec4(inx, iny, inz, inw)
        let out = inv * inp
        let out = out / out.w
        vec3(out.x, out.y, out.z)

    ///
    /// frustum matrix (tested)
    /// @param lbn left/bottom/near
    /// @param rtf right/top/far
    /// @return the frustum matrix
    ///
    static member frustum(lbn: vec3, rtf: vec3) =
        let width   = rtf.x - lbn.x
        let height  = rtf.y - lbn.y
        let depth   = rtf.z - lbn.z
        let a   = (rtf.x + lbn.x) / width
        let b   = (rtf.y + lbn.y) / height
        let c   = -(rtf.z + lbn.z) / depth
        let d   = -(2.0f * rtf.z * lbn.z) / depth

        mat4(2.0f * lbn.z / width, 0.0f, 0.0f, 0.0f,
            0.0f, 2.0f * lbn.z / height, 0.0f, 0.0f,
            a, b, c, -1.0f,
            0.0f, 0.0f, d, 0.0f)


    ///
    /// perspective projection matrix (tested)
    /// @param fovy field of view in radians and in y direction
    /// @param aspect the aspect ratio
    /// @param near near plane value
    /// @param far far plane value
    ///
    static member perspective(fovy: single, aspect: single, near: single, far: single) =
        let f       = 1.0f / (System.Math.Tan ((fovy * 0.5f) |> float) |> single)
        let denom   = near - far
        let a       = (far + near) / denom
        let b       = (2.0f * far * near) / denom

        mat4(f / aspect, 0.0f, 0.0f, 0.0f,
            0.0f, f, 0.0f, 0.0f,
            0.0f, 0.0f, a, -1.0f,
            0.0f, 0.0f, b, 0.0f)

    ///
    /// lookat matrix (tested)
    /// @param eye the eye position
    /// @param dest the eye destination position
    /// @param up the up vector
    /// @return the lookat matrix
    ///
    static member lookat(eye: vec3, dest: vec3, up: vec3) =
        let f   = (dest - eye)       |> vec3.normalize
        let s   = vec3.cross (f, up) |> vec3.normalize
        let u   = vec3.cross (s, f)  |> vec3.normalize

        let trans   = mat4.translation (-eye)

        let m   =
            mat4(s.x, u.x, -f.x, 0.0f,
                s.y, u.y, -f.y, 0.0f,
                s.z, u.z, -f.z, 0.0f,
                0.0f, 0.0f, 0.0f, 1.0f)
        m * trans

    ///
    /// @brief convert from world coordinates to local coordinates
    /// @param m world matrix
    /// @param in the input world coordinates
    /// @return local coordinates
    ///
    static member worldToLocal(world: mat4, inp: vec3) =
        let inv_world   = mat4.inverse world
        let vin     = vec4(inp.x, inp.y, inp.z, 1.0f)
        let vout    = inv_world * vin
        vec3(vout.x, vout.y, vout.z)

    ///
    /// @brief transform a vec3 by a mat4
    /// @param m matrix
    /// @param in input vector
    /// @return the transformed vector
    ///
    static member transform(m: mat4, inp: vec3) =
        let vin     = vec4(inp.x, inp.y, inp.z, 1.0f)
        let vout    = m * vin
        vec3(vout.x / vout.w, vout.y / vout.w, vout.z / vout.w)

    ///
    /// @brief transform a vec4 by a mat4
    /// @param m matrix
    /// @param in input vector
    /// @return the transformed vector
    ///
    static member transform(m: mat4, inp: vec4) = m * inp

    static member decompose(m: mat4) =
        let col0    = vec3(m.c0.x, m.c0.y, m.c0.z)
        let col1    = vec3(m.c1.x, m.c1.y, m.c1.z)
        let col2    = vec3(m.c2.x, m.c2.y, m.c2.z)
        let det     = mat4.determinant m

        let scale   = vec3(vec4.length m.c0, vec4.length m.c1, vec4.length m.c2)
        let trans   = vec3(m.c3.x, m.c3.y, m.c3.z)

        let scale   = if det < 0.0f then -scale else scale

        let col0    = if scale.x <> 0.0f then col0 / scale.x else col0
        let col1    = if scale.y <> 0.0f then col1 / scale.y else col1
        let col2    = if scale.z <> 0.0f then col2 / scale.z else col2

        let rot_matrix =
            mat3(col0.x, col0.y, col0.z,
                col1.x, col1.y, col1.z,
                col2.x, col2.y, col2.z)

        let rot = quat.ofMat3 rot_matrix

        scale, trans, rot


