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
    static member inline length (v: vec2)              = vec2.dot(v, v) |> sqrt
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
    static member inline length (v: vec3)              = vec3.dot(v, v) |> sqrt
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
    static member inline length (v: vec4)              = vec4.dot(v, v) |> sqrt
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
    static member inline length (v: ivec2)               = ivec2.dot(v, v) |> single |> sqrt |> int
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
    static member inline length (v: ivec3)                  = ivec3.dot(v, v) |> single |> sqrt |> int
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
    static member inline length (v: ivec4)               = ivec4.dot(v, v) |> single |> sqrt |> int
    static member inline distance (v0: ivec4, v1: ivec4) = let s = v1 - v0 in ivec4.length s
    static member inline normalize (v: ivec4)            = let l = ivec4.length v in v / l   

    static member inline min (a: ivec4) (b: ivec4)    = ivec4(min a.x b.x, min a.y b.y, min a.z b.z, min a.w b.w)
    static member inline max (a: ivec4) (b: ivec4)    = ivec4(max a.x b.x, max a.y b.y, max a.z b.z, max a.w b.w)
    end

type dvec2 = struct
    val         x   : double
    val         y   : double

    new(x, y) = { x = x; y = y }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | _ -> failwith "vec2: index out of range"

    static member inline (+) (a: dvec2, b: dvec2)    = dvec2(a.x + b.x, a.y + b.y)
    static member inline (-) (a: dvec2, b: dvec2)    = dvec2(a.x - b.x, a.y - b.y)
    static member inline (*) (a: dvec2, b: dvec2)    = dvec2(a.x * b.x, a.y * b.y)
    static member inline (/) (a: dvec2, b: dvec2)    = dvec2(a.x / b.x, a.y / b.y)

    static member inline (*) (a: dvec2, b: double)  = dvec2(a.x * b, a.y * b)
    static member inline (/) (a: dvec2, b: double)  = dvec2(a.x / b, a.y / b)

    static member inline (*) (a: double, b: dvec2)  = dvec2(a * b.x, a * b.y)
    static member inline (/) (a: double, b: dvec2)  = dvec2(a / b.x, a / b.y)

    static member inline (~-) (a: dvec2)            = dvec2(-a.x, -a.y)

    static member inline dot (a: dvec2, b: dvec2)        =  a.x * b.x + a.y * b.y
    static member inline length (v: dvec2)              = dvec2.dot(v, v) |> sqrt
    static member inline distance (v0: dvec2, v1: dvec2) = let s = v1 - v0 in dvec2.length s
    static member inline normalize (v: dvec2)           = let l = dvec2.length v in v / l

    static member inline orhtogonal (a: dvec2)      = dvec2(-a.y, a.x)
    static member inline orthogonal2 (a: dvec2)     = dvec2(a.y, -a.x)

    static member inline min (a: dvec2) (b: dvec2)    = dvec2(min a.x b.x, min a.y b.y)
    static member inline max (a: dvec2) (b: dvec2)    = dvec2(max a.x b.x, max a.y b.y)

    override x.ToString() = sprintf "<%f, %f>" x.x x.y

    end

type dvec3 = struct
    val         x   : double
    val         y   : double
    val         z   : double
    
    new(x, y, z) = { x = x; y = y; z = z }
    new(v2: dvec2, z) = { x = v2.x; y = v2.y; z = z }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | _ -> failwith "vec3: index out of range"

    member x.xy = dvec2(x.x, x.y)

    static member inline (+) (a: dvec3, b: dvec3)    = dvec3(a.x + b.x, a.y + b.y, a.z + b.z)
    static member inline (-) (a: dvec3, b: dvec3)    = dvec3(a.x - b.x, a.y - b.y, a.z - b.z)
    static member inline (*) (a: dvec3, b: dvec3)    = dvec3(a.x * b.x, a.y * b.y, a.z * b.z)
    static member inline (/) (a: dvec3, b: dvec3)    = dvec3(a.x / b.x, a.y / b.y, a.z / b.z)

    static member inline (*) (a: dvec3, b: double)  = dvec3(a.x * b, a.y * b, a.z * b)
    static member inline (/) (a: dvec3, b: double)  = dvec3(a.x / b, a.y / b, a.z / b)

    static member inline (*) (a: double, b: dvec3)  = dvec3(a * b.x, a * b.y, a * b.z)
    static member inline (/) (a: double, b: dvec3)  = dvec3(a / b.x, a / b.y, a / b.z)

    static member inline (~-) (a: dvec3)            = dvec3(-a.x, -a.y, -a.z)

    static member inline dot (a: dvec3, b: dvec3)        =  a.x * b.x + a.y * b.y + a.z * b.z
    static member inline length (v: dvec3)              = dvec3.dot(v, v) |> sqrt
    static member inline distance (v0: dvec3, v1: dvec3) = let s = v1 - v0 in dvec3.length s
    static member inline normalize (v: dvec3)           = let l = dvec3.length v in v / l

    static member inline cross (a: dvec3, b: dvec3)  = dvec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)

    static member inline min (a: dvec3) (b: dvec3)    = dvec3(min a.x b.x, min a.y b.y, min a.z b.z)
    static member inline max (a: dvec3) (b: dvec3)    = dvec3(max a.x b.x, max a.y b.y, max a.z b.z)

    override x.ToString() = sprintf "<%f, %f, %f>" x.x x.y x.z

    end

type dvec4 = struct
    val         x   : double
    val         y   : double
    val         z   : double
    val         w   : double
    
    new(x, y, z, w) = { x = x; y = y; z = z; w = w }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | 3 -> x.w
        | _ -> failwith "vec4: index out of range"

    member x.toArray () = [| x.x; x.y; x.z; x.w |]

    static member inline (+) (a: dvec4, b: dvec4)    = dvec4(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
    static member inline (-) (a: dvec4, b: dvec4)    = dvec4(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
    static member inline (*) (a: dvec4, b: dvec4)    = dvec4(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
    static member inline (/) (a: dvec4, b: dvec4)    = dvec4(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w)

    static member inline (*) (a: dvec4, b: double)  = dvec4(a.x * b, a.y * b, a.z * b, a.w * b)
    static member inline (/) (a: dvec4, b: double)  = dvec4(a.x / b, a.y / b, a.z / b, a.w / b)

    static member inline (*) (a: double, b: dvec4)  = dvec4(a * b.x, a * b.y, a * b.z, a * b.w)
    static member inline (/) (a: double, b: dvec4)  = dvec4(a / b.x, a / b.y, a / b.z, a / b.w)

    static member inline (~-) (a: dvec4)            = dvec4(-a.x, -a.y, -a.z, -a.w)

    static member inline dot (a: dvec4, b: dvec4)        =  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w
    static member inline length (v: dvec4)              = dvec4.dot(v, v) |> sqrt
    static member inline distance (v0: dvec4, v1: dvec4) = let s = v1 - v0 in dvec4.length s
    static member inline normalize (v: dvec4)           = let l = dvec4.length v in v / l

    static member inline min (a: dvec4) (b: dvec4)    = dvec4(min a.x b.x, min a.y b.y, min a.z b.z, min a.w b.w)
    static member inline max (a: dvec4) (b: dvec4)    = dvec4(max a.x b.x, max a.y b.y, max a.z b.z, max a.w b.w)
    end

type color3 = struct
    val         r   : single
    val         g   : single
    val         b   : single
    
    new(r, g, b) = { r = r; g = g; b = b }

    member x.Item i =
        match i with
        | 0 -> x.r
        | 1 -> x.g
        | 2 -> x.b
        | _ -> failwith "color3: index out of range"

    static member inline (+) (a: color3, b: color3)    = color3(a.r + b.r, a.g + b.g, a.b + b.b)
    static member inline (-) (a: color3, b: color3)    = color3(a.r - b.r, a.g - b.g, a.b - b.b)
    static member inline (*) (a: color3, b: color3)    = color3(a.r * b.r, a.g * b.g, a.b * b.b)
    static member inline (/) (a: color3, b: color3)    = color3(a.r / b.r, a.g / b.g, a.b / b.b)

    static member inline (*) (a: color3, b: single)    = color3(a.r * b, a.g * b, a.b * b)
    static member inline (/) (a: color3, b: single)    = color3(a.r / b, a.g / b, a.b / b)

    static member inline normalize (v: color3)         = let l = vec3.length (vec3 (v.r, v.g, v.b)) in v / l   
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

    static member inline normalize (v: color4)         = let l = vec4.length (vec4 (v.r, v.g, v.b, v.a)) in v / l   
    end

type icolor3 = struct
    val         r   : uint8
    val         g   : uint8
    val         b   : uint8
    
    new(r, g, b) = { r = r; g = g; b = b }

    member x.Item i =
        match i with
        | 0 -> x.r
        | 1 -> x.g
        | 2 -> x.b
        | _ -> failwith "icolor3: index out of range"

    static member inline (+) (a: icolor3, b: icolor3)    = icolor3(a.r + b.r, a.g + b.g, a.b + b.b)
    static member inline (-) (a: icolor3, b: icolor3)    = icolor3(a.r - b.r, a.g - b.g, a.b - b.b)
    static member inline (*) (a: icolor3, b: icolor3)    = icolor3(a.r * b.r, a.g * b.g, a.b * b.b)
    static member inline (/) (a: icolor3, b: icolor3)    = icolor3(a.r / b.r, a.g / b.g, a.b / b.b)
    static member inline (*) (a: icolor3, b: uint8)      = icolor3(a.r * b, a.g * b, a.b * b)
    static member inline (/) (a: icolor3, b: uint8)      = icolor3(a.r / b, a.g / b, a.b / b)

    end

type icolor4 = struct
    val         r   : uint8
    val         g   : uint8
    val         b   : uint8
    val         a   : uint8
    
    new(r, g, b, a) = { r = r; g = g; b = b; a = a; }

    member x.Item i =
        match i with
        | 0 -> x.r
        | 1 -> x.g
        | 2 -> x.b
        | 3 -> x.a
        | _ -> failwith "icolor4: index out of range"

    static member inline (+) (a: icolor4, b: icolor4)    = icolor4(a.r + b.r, a.g + b.g, a.b + b.b, a.a + b.a)
    static member inline (-) (a: icolor4, b: icolor4)    = icolor4(a.r - b.r, a.g - b.g, a.b - b.b, a.a - b.a)
    static member inline (*) (a: icolor4, b: icolor4)    = icolor4(a.r * b.r, a.g * b.g, a.b * b.b, a.a * b.a)
    static member inline (/) (a: icolor4, b: icolor4)    = icolor4(a.r / b.r, a.g / b.g, a.b / b.b, a.a / b.a)
    static member inline (*) (a: icolor4, b: uint8)      = icolor4(a.r * b, a.g * b, a.b * b, a.a * b)
    static member inline (/) (a: icolor4, b: uint8)      = icolor4(a.r / b, a.g / b, a.b / b, a.a / b)
    end