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

module FsMath3D.Queries

open FsMath3D.Vector
open FsMath3D.Matrix
open FsMath3D.Quaternion
open FsMath3D.Transform
open FsMath3D.Geometry

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


    

