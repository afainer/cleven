// -*- c -*-
//
// Copyright (c) 2015-2016, Andrey Fainer <fandrey@gmx.com>
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#version 330

const float tex_tile_size = #.(float (tex-tile-size) 0f0)f;
const float voxsize  = 1.f / #.(max-tex-size).f;

in vec2 position;

uniform float camera_far_plane, tex_screen_size;
uniform mat4 camera_world_matrix;
out vec3 texcoord;

void main()
{
  texcoord =
    (vec4( position * tex_screen_size,
           (camera_far_plane - gl_InstanceID) * voxsize, 1.0f ) *
     camera_world_matrix).xyz + tex_tile_size / 2.0f;

  gl_Position = vec4( position, 0.0f, 1.0f );
}
