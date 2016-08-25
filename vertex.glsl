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

const int maxtexnum = #.(max-tex-attribs);

in  vec3 position;
in  vec3 texcoordsv[ maxtexnum ];
out vec3 texcoords[ maxtexnum ];

uniform mat4 camera;
uniform int  texnum;

void main()
{
  gl_Position = camera * vec4( position.x, position.y, position.z, 1 );

  for( int i = 0; i < texnum; ++i )
    texcoords[ i ] = texcoordsv[ i ];
}
