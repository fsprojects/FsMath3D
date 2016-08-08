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

has the following layout in memory: `[m0 m1 m2 m3 m4 m5 m6 m7 m8]`


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
