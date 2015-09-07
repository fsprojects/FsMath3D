(*
** Math3D for F#
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

module FsMath3D.Vector

open System
open System.Runtime
open System.Runtime.InteropServices

#nowarn "9"

[<StructAttribute; StructLayoutAttribute(LayoutKind.Sequential)>]
type vec2 =
    val         x   : single
    val         y   : single

    new(x, y) = { x = x; y = y }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | _ -> failwith "vec2: index out of range"

    static member (+) (a: vec2, b: vec2)    = vec2(a.x + b.x, a.y + b.y)
    static member (-) (a: vec2, b: vec2)    = vec2(a.x - b.x, a.y - b.y)
    static member (*) (a: vec2, b: vec2)    = vec2(a.x * b.x, a.y * b.y)
    static member (/) (a: vec2, b: vec2)    = vec2(a.x / b.x, a.y / b.y)

    static member (*) (a: vec2, b: single)  = vec2(a.x * b, a.y * b)
    static member (/) (a: vec2, b: single)  = vec2(a.x / b, a.y / b)

    static member dot (a: vec2, b: vec2)        =  a.x * b.x + a.y * b.y
    static member length (v: vec2)              = Math.Sqrt(vec2.dot(v, v) |> float) |> single
    static member distance (v0: vec2, v1: vec2) = let s = v1 - v0 in vec2.length s
    static member normalize (v: vec2)           = let l = vec2.length v in v / l

    static member orhtogonal (a: vec2)      = vec2(-a.y, a.x)
    static member orthogonal2 (a: vec2)     = vec2(a.y, -a.x)

[<StructAttribute; StructLayoutAttribute(LayoutKind.Sequential)>]
type vec3 =
    val         x   : single
    val         y   : single
    val         z   : single
    
    new(x, y, z) = { x = x; y = y; z = z }
    new(v2: vec2, z) = { x = v2.x; y = v2.y; z = z }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | _ -> failwith "vec3: index out of range"

    member x.xy = vec2(x.x, x.y)

    static member (+) (a: vec3, b: vec3)    = vec3(a.x + b.x, a.y + b.y, a.z + b.z)
    static member (-) (a: vec3, b: vec3)    = vec3(a.x - b.x, a.y - b.y, a.z - b.z)
    static member (*) (a: vec3, b: vec3)    = vec3(a.x * b.x, a.y * b.y, a.z * b.z)
    static member (/) (a: vec3, b: vec3)    = vec3(a.x / b.x, a.y / b.y, a.z / b.z)

    static member (*) (a: vec3, b: single)  = vec3(a.x * b, a.y * b, a.z * b)
    static member (/) (a: vec3, b: single)  = vec3(a.x / b, a.y / b, a.z / b)

    static member dot (a: vec3, b: vec3)        =  a.x * b.x + a.y * b.y + a.z * b.z
    static member length (v: vec3)              = Math.Sqrt(vec3.dot(v, v) |> float) |> single
    static member distance (v0: vec3, v1: vec3) = let s = v1 - v0 in vec3.length s
    static member normalize (v: vec3)           = let l = vec3.length v in v / l

    static member cross (a: vec3, b: vec3)  = vec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)

[<StructAttribute; StructLayoutAttribute(LayoutKind.Sequential)>]
type vec4 =
    val         x   : single
    val         y   : single
    val         z   : single
    val         w   : single
    
    new(x, y, z, w) = { x = x; y = y; z = z; w = w }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | 3 -> x.w
        | _ -> failwith "vec4: index out of range"

    static member (+) (a: vec4, b: vec4)    = vec4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
    static member (-) (a: vec4, b: vec4)    = vec4(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
    static member (*) (a: vec4, b: vec4)    = vec4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
    static member (/) (a: vec4, b: vec4)    = vec4(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w)

    static member (*) (a: vec4, b: single)  = vec4(a.x * b, a.y * b, a.z * b, a.w * b)
    static member (/) (a: vec4, b: single)  = vec4(a.x / b, a.y / b, a.z / b, a.w / b)

    static member dot (a: vec4, b: vec4)        =  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    static member length (v: vec4)              = Math.Sqrt(vec4.dot(v, v) |> float) |> single
    static member distance (v0: vec4, v1: vec4) = let s = v1 - v0 in vec4.length s
    static member normalize (v: vec4)           = let l = vec4.length v in v / l   
