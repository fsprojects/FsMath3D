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

module FsMath3D.Vector

open System
open System.Runtime
open System.Runtime.InteropServices

type vec2 = struct
    val         x   : single
    val         y   : single

    new(x, y) = { x = x; y = y }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | _ -> failwith "vec2: index out of range"

    static member inline (+) (a: vec2, b: vec2)    = vec2(a.x + b.x, a.y + b.y)
    static member inline (-) (a: vec2, b: vec2)    = vec2(a.x - b.x, a.y - b.y)
    static member inline (*) (a: vec2, b: vec2)    = vec2(a.x * b.x, a.y * b.y)
    static member inline (/) (a: vec2, b: vec2)    = vec2(a.x / b.x, a.y / b.y)

    static member inline (*) (a: vec2, b: single)  = vec2(a.x * b, a.y * b)
    static member inline (/) (a: vec2, b: single)  = vec2(a.x / b, a.y / b)

    static member inline (*) (a: single, b: vec2)  = vec2(a * b.x, a * b.y)
    static member inline (/) (a: single, b: vec2)  = vec2(a / b.x, a / b.y)

    static member inline (~-) (a: vec2)            = vec2(-a.x, -a.y)

    static member inline dot (a: vec2, b: vec2)        =  a.x * b.x + a.y * b.y
    static member inline length (v: vec2)              = Math.Sqrt(vec2.dot(v, v) |> float) |> single
    static member inline distance (v0: vec2, v1: vec2) = let s = v1 - v0 in vec2.length s
    static member inline normalize (v: vec2)           = let l = vec2.length v in v / l

    static member inline orhtogonal (a: vec2)      = vec2(-a.y, a.x)
    static member inline orthogonal2 (a: vec2)     = vec2(a.y, -a.x)

    static member inline min (a: vec2) (b: vec2)    = vec2(min a.x b.x, min a.y b.y)
    static member inline max (a: vec2) (b: vec2)    = vec2(max a.x b.x, max a.y b.y)

    override x.ToString() = sprintf "<%f, %f>" x.x x.y

    end

type vec3 = struct
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

    static member inline (+) (a: vec3, b: vec3)    = vec3(a.x + b.x, a.y + b.y, a.z + b.z)
    static member inline (-) (a: vec3, b: vec3)    = vec3(a.x - b.x, a.y - b.y, a.z - b.z)
    static member inline (*) (a: vec3, b: vec3)    = vec3(a.x * b.x, a.y * b.y, a.z * b.z)
    static member inline (/) (a: vec3, b: vec3)    = vec3(a.x / b.x, a.y / b.y, a.z / b.z)

    static member inline (*) (a: vec3, b: single)  = vec3(a.x * b, a.y * b, a.z * b)
    static member inline (/) (a: vec3, b: single)  = vec3(a.x / b, a.y / b, a.z / b)

    static member inline (*) (a: single, b: vec3)  = vec3(a * b.x, a * b.y, a * b.z)
    static member inline (/) (a: single, b: vec3)  = vec3(a / b.x, a / b.y, a / b.z)

    static member inline (~-) (a: vec3)            = vec3(-a.x, -a.y, -a.z)

    static member inline dot (a: vec3, b: vec3)        =  a.x * b.x + a.y * b.y + a.z * b.z
    static member inline length (v: vec3)              = Math.Sqrt(vec3.dot(v, v) |> float) |> single
    static member inline distance (v0: vec3, v1: vec3) = let s = v1 - v0 in vec3.length s
    static member inline normalize (v: vec3)           = let l = vec3.length v in v / l

    static member inline cross (a: vec3, b: vec3)  = vec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)

    static member inline min (a: vec3) (b: vec3)    = vec3(min a.x b.x, min a.y b.y, min a.z b.z)
    static member inline max (a: vec3) (b: vec3)    = vec3(max a.x b.x, max a.y b.y, max a.z b.z)

    override x.ToString() = sprintf "<%f, %f, %f>" x.x x.y x.z

    end

type vec4 = struct
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

    member x.toArray () = [| x.x; x.y; x.z; x.w |]

    static member inline (+) (a: vec4, b: vec4)    = vec4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
    static member inline (-) (a: vec4, b: vec4)    = vec4(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
    static member inline (*) (a: vec4, b: vec4)    = vec4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
    static member inline (/) (a: vec4, b: vec4)    = vec4(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w)

    static member inline (*) (a: vec4, b: single)  = vec4(a.x * b, a.y * b, a.z * b, a.w * b)
    static member inline (/) (a: vec4, b: single)  = vec4(a.x / b, a.y / b, a.z / b, a.w / b)

    static member inline (*) (a: single, b: vec4)  = vec4(a * b.x, a * b.y, a * b.z, a * b.w)
    static member inline (/) (a: single, b: vec4)  = vec4(a / b.x, a / b.y, a / b.z, a / b.w)

    static member inline (~-) (a: vec4)            = vec4(-a.x, -a.y, -a.z, -a.w)

    static member inline dot (a: vec4, b: vec4)        =  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    static member inline length (v: vec4)              = Math.Sqrt(vec4.dot(v, v) |> float) |> single
    static member inline distance (v0: vec4, v1: vec4) = let s = v1 - v0 in vec4.length s
    static member inline normalize (v: vec4)           = let l = vec4.length v in v / l

    static member inline min (a: vec4) (b: vec4)    = vec4(min a.x b.x, min a.y b.y, min a.z b.z, min a.w b.w)
    static member inline max (a: vec4) (b: vec4)    = vec4(max a.x b.x, max a.y b.y, max a.z b.z, max a.w b.w)
    end


type ivec2 = struct
    val         x   : int
    val         y   : int

    new(x, y) = { x = x; y = y }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | _ -> failwith "ivec2: index out of range"

    static member inline (+) (a: ivec2, b: ivec2)    = ivec2(a.x + b.x, a.y + b.y)
    static member inline (-) (a: ivec2, b: ivec2)    = ivec2(a.x - b.x, a.y - b.y)
    static member inline (*) (a: ivec2, b: ivec2)    = ivec2(a.x * b.x, a.y * b.y)
    static member inline (/) (a: ivec2, b: ivec2)    = ivec2(a.x / b.x, a.y / b.y)

    static member inline (*) (a: ivec2, b: int)  = ivec2(a.x * b, a.y * b)
    static member inline (/) (a: ivec2, b: int)  = ivec2(a.x / b, a.y / b)

    static member inline (*) (a: int, b: ivec2)  = ivec2(a * b.x, a * b.y)
    static member inline (/) (a: int, b: ivec2)  = ivec2(a / b.x, a / b.y)

    static member inline (~-) (a: ivec2)            = ivec2(-a.x, -a.y)

    static member inline dot (a: ivec2, b: ivec2)        =  a.x * b.x + a.y * b.y
    static member inline length (v: ivec2)               = Math.Sqrt(ivec2.dot(v, v) |> float) |> int
    static member inline distance (v0: ivec2, v1: ivec2) = let s = v1 - v0 in ivec2.length s
    static member inline normalize (v: ivec2)            = let l = ivec2.length v in v / l

    static member inline orhtogonal (a: ivec2)      = ivec2(-a.y, a.x)
    static member inline orthogonal2 (a: ivec2)     = ivec2(a.y, -a.x)

    static member inline min (a: ivec2) (b: ivec2)   = ivec2(min a.x b.x, min a.y b.y)
    static member inline max (a: ivec2) (b: ivec2)   = ivec2(max a.x b.x, max a.y b.y)
    end

type ivec3 = struct
    val         x   : int
    val         y   : int
    val         z   : int
    
    new(x, y, z) = { x = x; y = y; z = z }
    new(v2: ivec2, z) = { x = v2.x; y = v2.y; z = z }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | _ -> failwith "ivec3: index out of range"

    member x.xy = ivec2(x.x, x.y)

    static member inline (+) (a: ivec3, b: ivec3)   = ivec3(a.x + b.x, a.y + b.y, a.z + b.z)
    static member inline (-) (a: ivec3, b: ivec3)   = ivec3(a.x - b.x, a.y - b.y, a.z - b.z)
    static member inline (*) (a: ivec3, b: ivec3)   = ivec3(a.x * b.x, a.y * b.y, a.z * b.z)
    static member inline (/) (a: ivec3, b: ivec3)   = ivec3(a.x / b.x, a.y / b.y, a.z / b.z)

    static member inline (*) (a: ivec3, b: int)     = ivec3(a.x * b, a.y * b, a.z * b)
    static member inline (/) (a: ivec3, b: int)     = ivec3(a.x / b, a.y / b, a.z / b)

    static member inline (*) (a: int, b: ivec3)     = ivec3(a * b.x, a * b.y, a * b.z)
    static member inline (/) (a: int, b: ivec3)     = ivec3(a / b.x, a / b.y, a / b.z)

    static member inline (~-) (a: ivec3)            = ivec3(-a.x, -a.y, -a.z)

    static member inline dot (a: ivec3, b: ivec3)           =  a.x * b.x + a.y * b.y + a.z * b.z
    static member inline length (v: ivec3)                  = Math.Sqrt(ivec3.dot(v, v) |> float) |> int
    static member inline distance (v0: ivec3, v1: ivec3)    = let s = v1 - v0 in ivec3.length s
    static member inline normalize (v: ivec3)               = let l = ivec3.length v in v / l

    static member inline cross (a: ivec3, b: ivec3)  = ivec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
    
    static member inline min (a: ivec3) (b: ivec3)    = ivec3(min a.x b.x, min a.y b.y, min a.z b.z)
    static member inline max (a: ivec3) (b: ivec3)    = ivec3(max a.x b.x, max a.y b.y, max a.z b.z)
    end

type ivec4 = struct
    val         x   : int
    val         y   : int
    val         z   : int
    val         w   : int
    
    new(x, y, z, w) = { x = x; y = y; z = z; w = w }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | 3 -> x.w
        | _ -> failwith "ivec4: index out of range"

    static member inline (+) (a: ivec4, b: ivec4)    = ivec4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
    static member inline (-) (a: ivec4, b: ivec4)    = ivec4(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
    static member inline (*) (a: ivec4, b: ivec4)    = ivec4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
    static member inline (/) (a: ivec4, b: ivec4)    = ivec4(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w)

    static member inline (*) (a: ivec4, b: int)     = ivec4(a.x * b, a.y * b, a.z * b, a.w * b)
    static member inline (/) (a: ivec4, b: int)     = ivec4(a.x / b, a.y / b, a.z / b, a.w / b)

    static member inline (*) (a: int, b: ivec4)     = ivec4(a * b.x, a * b.y, a * b.z, a * b.w)
    static member inline (/) (a: int, b: ivec4)     = ivec4(a / b.x, a / b.y, a / b.z, a / b.w)

    static member inline (~-) (a: ivec4)            = ivec4(-a.x, -a.y, -a.z, -a.w)

    static member inline dot (a: ivec4, b: ivec4)        =  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    static member inline length (v: ivec4)               = Math.Sqrt(ivec4.dot(v, v) |> float) |> int
    static member inline distance (v0: ivec4, v1: ivec4) = let s = v1 - v0 in ivec4.length s
    static member inline normalize (v: ivec4)            = let l = ivec4.length v in v / l   

    static member inline min (a: ivec4) (b: ivec4)    = ivec4(min a.x b.x, min a.y b.y, min a.z b.z, min a.w b.w)
    static member inline max (a: ivec4) (b: ivec4)    = ivec4(max a.x b.x, max a.y b.y, max a.z b.z, max a.w b.w)
    end

type color4 = struct
    val         r   : single
    val         g   : single
    val         b   : single
    val         a   : single
    
    new(r, g, b, a) = { r = r; g = g; b = b; a = a; }

    member x.Item i =
        match i with
        | 0 -> x.r
        | 1 -> x.g
        | 2 -> x.b
        | 3 -> x.a
        | _ -> failwith "color4: index out of range"

    static member inline (+) (a: color4, b: color4)    = color4(a.r + b.r, a.g + b.g, a.b + b.b, a.a + b.a)
    static member inline (-) (a: color4, b: color4)    = color4(a.r - b.r, a.g - b.g, a.b - b.b, a.a - b.a)
    static member inline (*) (a: color4, b: color4)    = color4(a.r * b.r, a.g * b.g, a.b * b.b, a.a * b.a)
    static member inline (/) (a: color4, b: color4)    = color4(a.r / b.r, a.g / b.g, a.b / b.b, a.a / b.a)

    static member inline (*) (a: color4, b: single)    = color4(a.r * b, a.g * b, a.b * b, a.a * b)
    static member inline (/) (a: color4, b: single)    = color4(a.r / b, a.g / b, a.b / b, a.a / b)

    static member inline (~-) (a: color4)              = color4(-a.r, -a.g, -a.b, -a.a)

    static member inline normalize (v: color4)         = let l = vec4.length (vec4 (v.r, v.g, v.b, v.a)) in v / l   
    end