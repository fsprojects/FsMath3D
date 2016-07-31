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

module FsRTK.Math3D.Queries

open FsRTK.Math3D.Vector
open FsRTK.Math3D.Matrix
open FsRTK.Math3D.Quaternion
open FsRTK.Math3D.Transform
open FsRTK.Math3D.Geometry

type IntersectionResult =
    | Intersect of vec2 * vec3
    | Parallel
    | NoIntersection

// Ray-Triangle Intersection Test Routines         
// Different optimizations of my and Ben Trumbore's
// code from journals of graphics tools (JGT)      
// http://www.acm.org/jgt/                         
// by Tomas Moller, May 2000
let intersectRayTri3 (r: ray3) (t: tri3) =
        
    let EPSILON = 1.0f / (1024.0f * 1024.0f)

    // find vectors for two edges sharing vert0
    let edge1 = t.v1 - t.v0
    let edge2 = t.v2 - t.v0

    // begin calculating determinant - also used to calculate U parameter
    let pvec = vec3.cross(r.direction, edge2)

    // if determinant is near zero, ray lies in plane of triangle
    let det = vec3.dot(edge1, pvec)

    // calculate distance from vert0 to ray origin
    let tvec = r.start - t.v0
    let inv_det = 1.0f / det

    let qvec = vec3.cross(tvec, edge1)

    let computePoint(u, v) =
        let t = vec3.dot(edge2, qvec) * inv_det

        // u	*= inv_det;
        // v	*= inv_det;

        r.start + t * r.direction

    if det > -EPSILON && det < EPSILON
    then Parallel
    else
        let u = vec3.dot(tvec, pvec) * inv_det
        if u < 0.0f || u > 1.0f
        then NoIntersection
        else 
            // calculate V parameter and test bounds
            let v = vec3.dot(r.direction, qvec) * inv_det
            if v < 0.0f || u + v > 1.0f
            then NoIntersection
            else Intersect (vec2(u, v), computePoint(u, v))


    

