# FsMath3D
F# 3D Math Library for realtime applications

### Purpose
An F# 3D math library that can be used to make 3D games and applications. <b>FsMath3D</b> uses [OpenGL GLSL](https://www.khronos.org/files/opengles_shading_language.pdf) semantics and conventions, so it's easier to interface with OpenGL and OpenGL ES.

### Convention
The Matrices are written in Column major order (GLSL default). This means that the matrix: 

```math
| m0 m3 m6 |
| m1 m4 m7 |
| m2 m5 m8 |
```

has the following layout in memory: `[| m0 m1 m2 m3 m4 m5 m6 m7 m8 |]`

### Modules
The FsMath3D is composed of several modules:

* [<b>Vector</b>](./docs/Vector.md)
  - <b>`vec2, vec3, vec4`</b>: 2D, 3D and 4D single precision floating point vectors (32 bits per component)
  - <b>`ivec2, ivec3, ivec4`</b>: 2D, 3D and 4D signed integer vectors  (32 bits per component)
  - <b>`color4`</b>: rgba color with single precision floating point components

* [<b>Matrix</b>](./docs/Matrix.md): Contains <b>`mat2, mat3, mat4`</b>: 2x2, 3x3 and 4x4 single precision floating point matrices (32 bits per component)

* [<b>Quaternion</b>](./docs/Quaternion.md): For the `quat` type: a quaternion with 4 single precision floating point components.

* [<b>Transform</b>](./docs/Transform.md): GLU equivalent functions to transform vectors or create transformation matrices.

* [<b>Geometry</b>](./docs/Geometry.md): Geometric primitives:
  - <b>`size2`</b>: 2D size structure (single precision floating point components)
  - <b>`isize2`</b>: 2D size structure (signed int components)
  - <b>`tri16, tri32`</b>: vertices indices (16 bits unsigned short integer and 32 bit unsigned integer respectively)
  - <b>`rect2`</b>: 2D rectangle (single precision floating point components)
  - <b>`irect2`</b>: 2D rectangle (signed integer components)
  - <b>`ray3`</b>: 3D ray (single precision floating point components)
  - <b>`plane`</b>: 3D plane (single precision floating point components)
  - <b>`tri3`</b>: 3D triangle with 3 `vec3` to represent vertices.

* [<b>Queries</b>](./docs/Queries.md): Intersection and proximity geometric query functions.

### LICENSE

Copyright (C) 2015-2016  Wael El Oraiby

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see [GNU licenses](http://www.gnu.org/licenses/).

Under Section 7 of GPL version 3, you are granted additional permissions
described in the [GCC Runtime Library Exception](https://www.gnu.org/licenses/gcc-exception-3.1.en.html), version 3.1, as
published by the Free Software Foundation.

<b>Roughly speaking: You can statically link with the code but you are obliged
to make open your bug fixes and/or enhancement to the code.</b>
