# FsMath3D
**FsMath3D** is an F# 3D geometry library for games and other realtime 3D applications.

**FsMath3D** adheres to the [OpenGL GLSL](https://www.khronos.org/files/opengles_shading_language.pdf)
semantics and conventions, so that it's easier to interface with OpenGL and OpenGL ES.

### Storage Convention
Matrices are stored in contigous memory as 1D arrays of a corresponding type
in column-major order, which is the GLSL default. For example, the following matrix
```math
\begin{pmatrix}
a_1 & b_1 & c_1 \\
a_2 & b_2 & c_2 \\
a_3 & b_3 & c_3
\end{pmatrix}
```
is represented in memory as
```f#
[| a₁ ; a₂ ; a₃ ; b₁ ; b₂ ; b₃ ; c₁ ; c₂ ; c₃ |]
```

### Modules
**FsMath3D** consists of the following modules:

* [<b>Vector</b>](./docs/Vector.md).
  - <b>`vec2, vec3, vec4`</b>: 2D, 3D and 4D vectors, single-precision (32-bit) floating point.
  - <b>`ivec2, ivec3, ivec4`</b>: 2D, 3D and 4D vectors, signed 32-bit integer.
  - <b>`color4`</b>: rgba color with single-precision floating point components.

* [<b>Matrix</b>](./docs/Matrix.md).
<b>`mat2, mat3, mat4`</b>: Square 2×2, 3×3 and 4×4 single-precision floating point matrices.

* [<b>Quaternion</b>](./docs/Quaternion.md).
The **`quat`** type: the quaternion with 4 single-precision floating point components.

* [<b>Transform</b>](./docs/Transform.md).
GLU equivalent functions to transform vectors or create transformation matrices.

* [<b>Geometry</b>](./docs/Geometry.md): Geometric primitives:
  - <b>`size2`</b>: 2D size structure, single-precision floating point.
  - <b>`isize2`</b>: 2D size structure, signed 32-bit integer.
  - <b>`tri16, tri32`</b>: vertex indices, 16- and 32-bit unsigned integers.
  - <b>`rect2`</b>: 2D rectangle, single-precision floating point.
  - <b>`irect2`</b>: 2D rectangle, signed 32-bit integer.
  - <b>`ray3`</b>: 3D ray, single-precision floating point.
  - <b>`plane`</b>: 3D plane, single-precision floating point.
  - <b>`tri3`</b>: 3D triangle with three `vec3` vertices.

* [<b>Queries</b>](./docs/Queries.md). Intersection and proximity geometric query functions.

### LICENSE

**FsMath3D** is licensed under the terms of the [New BSD Three-Clause License](./LICENSE)
([`SPDX-License-Identifier:`](https://spdx.dev/ids/)`BSD-3-Clause`),
permitting the library's use and redistribution, contingent on license compliance,
in both commercial and non-commercial applications, either binary-only or open-source.

Copyright © 2015‒2018 Wael El Oraiby
