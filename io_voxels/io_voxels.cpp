// -*- mode: c++; coding: utf-8 -*-
//
// Copyright (c) 2015, Andrey Fainer <fandrey@gmx.com>
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <vcg/space/index/grid_static_ptr.h>
#include <vcg/simplex/face/component_ep.h>
#include <vcg/complex/complex.h>
#include <vcg/complex/algorithms/update/component_ep.h>

#include "io_voxels.h"

// vcg::tri::Inside::Is_Inside uses old name of GetClosestFaceEP
namespace vcg
{
  namespace tri
  {

    template <class MESH, class GRID>
    typename MESH::FaceType * GetClosestFace( MESH & mesh, GRID & gr, const typename GRID::CoordType & _p,
                                              const typename GRID::ScalarType & _maxDist, typename GRID::ScalarType & _minDist,
                                              typename GRID::CoordType & _closestPt, typename GRID::CoordType & _normf,
                                              typename GRID::CoordType & _ip)
    {
      return GetClosestFaceEP( mesh,
                               gr,
                               _p,
                               _maxDist,
                               _minDist,
                               _closestPt,
                               _normf,
                               _ip);
    }
  }
}

#include <vcg/complex/algorithms/inside.h>

class FaceEP;
struct UsedTypes : public vcg::UsedTypes< vcg::Use<FaceEP>::AsFaceType,
                                          vcg::Use<CVertexO>::AsVertexType>
                                          {};

class FaceEP : public vcg::Face< UsedTypes,
                                 vcg::face::VertexRef,
                                 vcg::face::Normal3f,
                                 vcg::face::BitFlags,
                                 vcg::face::Mark,
                                 vcg::face::EdgePlane>
{};

class MeshEP : public vcg::tri::TriMesh< std::vector<CVertexO>,
                                         std::vector<FaceEP> >
{};

typedef vcg::GridStaticPtr<FaceEP, FaceEP::ScalarType> TriMeshGrid;

// At the moment we have just one voxel format: RGBA.
const size_t voxel_size = 4;

const size_t rasterization_scale = 4;

size_t tile_size = 64 * rasterization_scale;

static vcg::Point3i voxmap_size;

QList< MeshIOInterface::Format > VoxelsIOPlugin::importFormats() const
{
   return QList< Format >(); // export only
}

QList< MeshIOInterface::Format > VoxelsIOPlugin::exportFormats() const
{
  QList< Format > formatList;
  formatList << Format( "Voxel map", tr("voxels") );
  return formatList;
}

void VoxelsIOPlugin::GetExportMaskCapability(QString &format, int &capability, int &defaultBits) const
{
}

bool VoxelsIOPlugin::open(const QString &formatName, const QString &fileName, MeshModel &m, int& mask, const RichParameterSet & par,vcg::CallBackPos *cb, QWidget *parent)
{
   return false; // export only
}

static void init_voxmap_size( const MeshModel & m )
{
  int
    x = ceilf( m.cm.bbox.max.X() - m.cm.bbox.min.X() ),
    y = ceilf( m.cm.bbox.max.Y() - m.cm.bbox.min.Y() ),
    z = ceilf( m.cm.bbox.max.Z() - m.cm.bbox.min.Z() ),
    rx = x % tile_size,
    ry = y % tile_size,
    rz = z % tile_size;

  voxmap_size = vcg::Point3i( !rx ? x : x + tile_size - rx,
                              !ry ? y : y + tile_size - ry,
                              !rz ? z : z + tile_size - rz );
}

static void make_mesh( MeshModel & m, MeshEP * mesh )
{
  for( CMeshO::FaceIterator i = m.cm.face.begin(); i < m.cm.face.end(); i++ )
    {
      FaceEP f;
      f.V( 0 ) = i->V( 0 );
      f.V( 1 ) = i->V( 1 );
      f.V( 2 ) = i->V( 2 );
      f.ImportData( *i );
      mesh->face.push_back( f );
    }

  mesh->bbox = m.cm.bbox;

  vcg::tri::UpdateComponentEP<MeshEP>::Set( *mesh );
}

static unsigned char * rasterize( VoxelsIOPlugin * plugin, MeshModel & m )
{
  size_t voxels_num = voxmap_size.X() * voxmap_size.Y() * voxmap_size.Z();
  unsigned char * voxels = new unsigned char[ voxels_num * voxel_size ];

  if( !voxels )
    return NULL;

  memset( voxels, 0, voxels_num * voxel_size );

  vcg::Point3f
    diff( vcg::Point3f( voxmap_size.X() / 2.f,
                        voxmap_size.Y() / 2.f,
                        voxmap_size.Z() / 2.f ) - m.cm.bbox.Center() ),
    min( floorf( m.cm.bbox.min.X() ),
         floorf( m.cm.bbox.min.Y() ),
         floorf( m.cm.bbox.min.Z() ) ),
    max( ceilf( m.cm.bbox.max.X() ),
         ceilf( m.cm.bbox.max.Y() ),
         ceilf( m.cm.bbox.max.Z() ) );

  MeshEP mesh;
  make_mesh( m, &mesh );

  TriMeshGrid grid;
  grid.Set( mesh.face.begin(), mesh.face.end() );

  int slice_size = voxmap_size.Y() * voxmap_size.X();

  TriMeshGrid::CoordType xyz;
  for( xyz.Z() = min.Z() + .5f; xyz.Z() < max.Z(); xyz.Z() += 1.f )
    {
      for( xyz.Y() = min.Y() + .5f; xyz.Y() < max.Y(); xyz.Y() += 1.f )
        for( xyz.X() = min.X() + .5f; xyz.X() < max.X(); xyz.X() += 1.f )
          if( vcg::tri::Inside< TriMeshGrid, MeshEP >::Is_Inside( mesh,
                                                                  grid,
                                                                  xyz ) )
            {
              qint64 i =
                static_cast<int>(xyz.Z() + diff.Z()) * slice_size +
                static_cast<int>(xyz.Y() + diff.Y()) * voxmap_size.X() +
                static_cast<int>(xyz.X() + diff.X());

              memset( voxels + i * voxel_size, UCHAR_MAX, voxel_size );
            }

      std::cout << xyz.Z() << "/" << max.Z() << std::endl;
      plugin->Log( "%f/%f", xyz.Z(), max.Z() );
    }

  return voxels;
}

QString write_nrrd( unsigned char * voxmap )
{
  QFile nrrd( QString( "/tmp/%1.nrrd" )
              .arg( QCoreApplication::applicationPid() ) );

  if( !nrrd.open( QIODevice::WriteOnly | QIODevice::Truncate ) )
    return QString();

  {
    QTextStream str( &nrrd );
    str << "NRRD0001\ntype: unsigned char\ndimension: 4\nsizes: 4 "
        << voxmap_size.X() << " " << voxmap_size.Y() << " " << voxmap_size.Z()
        << "\nencoding: raw\n\n";
  }

  qint64 voxels_bytes =
    voxmap_size.X() * voxmap_size.Y() * voxmap_size.Z() * voxel_size;

  if( nrrd.write( reinterpret_cast< char * >( voxmap ),
                  voxels_bytes ) != voxels_bytes )
    return QString();

  return nrrd.fileName();
}

unsigned char * resample( const QString & nrrd )
{
  voxmap_size /= rasterization_scale;
  tile_size /= rasterization_scale;

  QProcess proc;
  proc.setProgram( "unu" );
  proc.setArguments( QStringList()
                     << "resample"
                     << "-k" << "cubic:1,0"
                     << "-i" << nrrd
                     << "-s" << "="
                     << QString::number( voxmap_size.X() )
                     << QString::number( voxmap_size.Y() )
                     << QString::number( voxmap_size.Z() )
                     << "-o" << nrrd + ".resampled" );

  proc.start();
  if( !proc.waitForStarted( -1 ) || !proc.waitForFinished( -1 ) )
    return NULL;

  qint64 bytes =
    voxmap_size.X() * voxmap_size.Y() * voxmap_size.Z() * voxel_size;

  char * voxels = new char[ bytes ];

  QFile nf( nrrd + ".resampled" );
  if( !voxels ||
      !nf.open( QIODevice::ReadOnly ) ||
      nf.size() <= bytes )
    return NULL;

  nf.seek( nf.size() - bytes );
  if( nf.read( voxels, bytes ) != bytes )
    return NULL;

  return reinterpret_cast< unsigned char *>( voxels );
}

/* origin is the location of the first tile voxel, i.e. left bottom corner. */
void tile( const unsigned char * voxels,
           const vcg::Point3i & origin,
           unsigned char * tile_voxels )
{
  const unsigned char
    * voxel = voxels + (origin.Z() * voxmap_size.Y() * voxmap_size.X() +
                        origin.Y() * voxmap_size.X() +
                        origin.X()) * voxel_size;

  size_t
    tile_row = tile_size * voxel_size,
    voxmap_row = voxmap_size.X() * voxel_size,
    voxmap_slice = (voxmap_size.Y() - tile_size) * voxmap_size.X() * voxel_size;

  for( size_t z = 0; z < tile_size; ++z, voxel += voxmap_slice )
    for( size_t y = 0;
         y < tile_size;
         ++y, voxel += voxmap_row, tile_voxels += tile_row )
      std::copy( voxel, voxel + tile_row, tile_voxels );
}

bool write_tiles( unsigned char * voxels,
                  const QString & voxname )
{
  qint64 tile_bytes = tile_size * tile_size * tile_size * voxel_size;

  QScopedArrayPointer< unsigned char >
    tile_voxels( new unsigned char[ tile_bytes ] );

  if( !tile_voxels )
    return false;

  QFile voxfile( voxname );
  if( !voxfile.open( QIODevice::WriteOnly ) )
    return false;

  vcg::Point3i xyz;
  for( xyz.Z() = 0; xyz.Z() < voxmap_size.Z(); xyz.Z() += tile_size )
    for( xyz.Y() = 0; xyz.Y() < voxmap_size.Y(); xyz.Y() += tile_size )
      for( xyz.X() = 0; xyz.X() < voxmap_size.X(); xyz.X() += tile_size )
        {
          tile( voxels, xyz, tile_voxels.data() );

          if( voxfile.write( reinterpret_cast< char * >( tile_voxels.data() ),
                             tile_bytes ) != tile_bytes )
            return false;
        }

  return true;
}

bool VoxelsIOPlugin::save(const QString &formatName, const QString &fileName, MeshModel &m, const int mask,const RichParameterSet & par, vcg::CallBackPos *cb, QWidget *parent)
{
  // TODO Add error messages

  vcg::tri::UpdatePosition< CMeshO >::Scale
    ( m.cm,
      static_cast< CMeshO::ScalarType >( rasterization_scale ) );

  vcg::tri::UpdateBounding< CMeshO >::Box( m.cm );

  init_voxmap_size( m );

  QScopedArrayPointer< unsigned char > voxels( rasterize( this, m ) );

  QString nrrd;

  if( !voxels ||
      (nrrd = write_nrrd( voxels.data() )).isEmpty() ||
      !(voxels.reset( resample( nrrd )), voxels) ||
      !write_tiles( voxels.data(), fileName ) )
    return false;

  QFile header( fileName.endsWith( ".voxels" ) ?
                fileName.left( fileName.length() -
                               QString( ".voxels" ).length() ) :
                fileName + ".header" );
  if( !header.open( QIODevice::WriteOnly ) )
    return false;

  QTextStream str( &header );
  str <<
    ";;; -*- lisp -*-\n(size " <<
    voxmap_size.X() << " " << voxmap_size.Y() << " " << voxmap_size.Z() <<
    ")\n(voxels . \"" << fileName << "\")\n";

  return true;
}

MESHLAB_PLUGIN_NAME_EXPORTER( VoxelsIOPlugin )
