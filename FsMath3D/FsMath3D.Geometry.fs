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

module FsMath3D.Geometry

open System
open System.Runtime
open System.Runtime.InteropServices

open FsMath3D.Vector

#nowarn "9"

[<StructAttribute; StructLayoutAttribute(LayoutKind.Sequential)>]
type size2 =
    val         width   : single
    val         height  : single

    new(w, h) = { width = w; height = h }

    member x.Item i =
        match i with
        | 0 -> x.width
        | 1 -> x.height
        | _ -> failwith "size2: index out of range"

    member x.AsVec2   = vec2(x.width, x.height)

    static member asVec2 (s: size2) = s.AsVec2
    static member ofVec2 (v: vec2)  = size2(v.x, v.y)

