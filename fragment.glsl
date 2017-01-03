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

in  vec3 texcoord;
out vec4 color;
uniform sampler3D sampler;

void main()
{
  if( texcoord.x < 0 ||
      texcoord.y < 0 ||
      texcoord.z < 0 ||
      texcoord.x > tex_tile_size ||
      texcoord.y > tex_tile_size ||
      texcoord.z > tex_tile_size )
    discard;

  color = texture( sampler, vec3(texcoord.xy, tex_tile_size - texcoord.z) );
}
