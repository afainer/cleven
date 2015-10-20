// -*- c -*-
//
// Copyright (c) 2015, Andrey Fainer <fandrey@gmx.com>
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

/* TODO Replace with patchable markers */
const int   maxtexnum = 15;
const float voxsize  = 1.f / 256.f;
const float voxsize2 = .5f / 256.f;
const float tex_tile_size = 64.f/256.f;
const vec3  tex_tiles_coords[] = vec3[]( vec3( 0.0f , 0.0f , 0.0f ),
                                         vec3( 0.25f, 0.0f , 0.0f ),
                                         vec3( 0.5f , 0.0f , 0.0f ),
                                         vec3( 0.75f, 0.0f , 0.0f ),
                                         vec3( 0.0f , 0.25f, 0.0f ),
                                         vec3( 0.25f, 0.25f, 0.0f ),
                                         vec3( 0.5f , 0.25f, 0.0f ),
                                         vec3( 0.75f, 0.25f, 0.0f ),
                                         vec3( 0.0f , 0.5f , 0.0f ),
                                         vec3( 0.25f, 0.5f , 0.0f ),
                                         vec3( 0.5f , 0.5f , 0.0f ),
                                         vec3( 0.75f, 0.5f , 0.0f ),
                                         vec3( 0.0f , 0.75f, 0.0f ),
                                         vec3( 0.25f, 0.75f, 0.0f ),
                                         vec3( 0.5f , 0.75f, 0.0f ) );

uniform sampler3D sampler;
uniform int       texnum;
uniform vec3 diffvecx[ maxtexnum ];
uniform vec3 diffvecy[ maxtexnum ];
uniform vec3 diffvecz[ maxtexnum ];

in  vec3 texcoords[ maxtexnum ];
out vec4 color;

bool out_tile( vec3 coord, vec3 tile_coord )
{
  vec3
    near = tile_coord,
    far  = tile_coord + tex_tile_size;

  if( coord.x < near.x || coord.x > far.x ||
      coord.y < near.y || coord.y > far.y ||
      coord.z < near.z || coord.z > far.z )
    return true;

  return false;
}

vec4 tile_texture( vec3 coord, vec3 tile_coord )
{
  vec3
    near = tile_coord + voxsize2,
    far  = tile_coord + tex_tile_size - voxsize2,
    /* Why 1.5? */
    forward =  vec3( coord.x >= near.x ? 0.0f : 1.5 * voxsize,
                     coord.y >= near.y ? 0.0f : 1.5 * voxsize,
                     coord.z >= near.z ? 0.0f : 1.5 * voxsize ),
    backward = vec3( coord.x <= far.x  ? 0.0f : 1.5 * voxsize,
                     coord.y <= far.y  ? 0.0f : 1.5 * voxsize,
                     coord.z <= far.z  ? 0.0f : 1.5 * voxsize );

  return texture( sampler, coord + forward - backward );
}

void main()
{
  vec4 c = vec4( 0, 0, 0, 0 ), xb = c, yb = c, zb = c, xf = c, yf = c, zf = c;

  for(int i = 0; i < texnum; ++i)
    {
      if( out_tile( texcoords[ i ], tex_tiles_coords[ i ] ) )
        continue;

      c += tile_texture( texcoords[ i ].xyz, tex_tiles_coords[ i ] );
      xb += tile_texture( vec3( texcoords[ i ].x - diffvecx[ i ].x,
                                texcoords[ i ].y - diffvecx[ i ].y,
                                texcoords[ i ].z - diffvecx[ i ].z ),
                          tex_tiles_coords[ i ] );
      yb += tile_texture( vec3( texcoords[ i ].x - diffvecy[ i ].x,
                                texcoords[ i ].y - diffvecy[ i ].y,
                                texcoords[ i ].z - diffvecy[ i ].z ),
                          tex_tiles_coords[ i ] );
      zb += tile_texture( vec3( texcoords[ i ].x - diffvecz[ i ].x,
                                texcoords[ i ].y - diffvecz[ i ].y,
                                texcoords[ i ].z - diffvecz[ i ].z),
                          tex_tiles_coords[ i ] );
      xf += tile_texture( vec3( texcoords[ i ].x + diffvecx[ i ].x,
                                texcoords[ i ].y + diffvecx[ i ].y,
                                texcoords[ i ].z + diffvecx[ i ].z ),
                          tex_tiles_coords[ i ] );
      yf += tile_texture( vec3( texcoords[ i ].x + diffvecy[ i ].x,
                                texcoords[ i ].y + diffvecy[ i ].y,
                                texcoords[ i ].z + diffvecy[ i ].z ),
                          tex_tiles_coords[ i ] );
      zf += tile_texture( vec3( texcoords[ i ].x + diffvecz[ i ].x,
                                texcoords[ i ].y + diffvecz[ i ].y,
                                texcoords[ i ].z + diffvecz[ i ].z),
                          tex_tiles_coords[ i ] );
    }

  vec3
    sun = normalize( vec3( .8, 1, -1 ) ),
    diff = vec3( (xf.a - xb.a) / 2.f,
                 (yf.a - yb.a) / 2.f,
                 (zf.a - zb.a) / 2.f );

  float d = length( diff ) < .0001f ? 1.0f : dot( sun, normalize( diff ) );

  color = vec4( c.rgb * max( d, 0 ), c.a);
}
