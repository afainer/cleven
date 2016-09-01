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

#include <btBulletDynamicsCommon.h>

#include <iostream>

struct voxel
{
  unsigned char r, g, b, a;
};

struct voxmap_size
{
  voxmap_size( unsigned int a, unsigned int b, unsigned int c )
    : x(a), y(b), z(c)
  {}

  unsigned int x, y, z;
};

// NB voxmap_tile_size and voxmap_tile_bytes dublicate definitions in
// voxel.lisp.

// Tile size for all voxmaps
const int voxmap_tile_size = 64;

// Tile size in bytes
const int voxmap_tile_bytes =
  sizeof( voxel ) * voxmap_tile_size * voxmap_tile_size * voxmap_tile_size;

void * (*detect_collision)();

ATTRIBUTE_ALIGNED16(class) VoxmapShape: public btBoxShape
{
 public:
  VoxmapShape( const btVector3& halfExtents, voxel ** ts )
    : btBoxShape( halfExtents )
    , tiles( ts )
    , size( halfExtents.x() * 2, halfExtents.y() * 2, halfExtents.z() * 2)
    {
      m_shapeType = CUSTOM_POLYHEDRAL_SHAPE_TYPE;
    }

  void setBoxShape()
  {
    m_shapeType = BOX_SHAPE_PROXYTYPE;
  }

  void setCustomShape()
  {
    m_shapeType = CUSTOM_POLYHEDRAL_SHAPE_TYPE;
  }

  //debugging
  virtual const char * getName() const
  {
    return "Voxmap";
  }

 private:
  voxel ** tiles;
  voxmap_size size;
};

class VoxmapsAsBoxes
{
public:
  VoxmapsAsBoxes( const btCollisionObjectWrapper * body0Wrap,
                  const btCollisionObjectWrapper * body1Wrap )
    : b0w( body0Wrap )
    , b1w( body1Wrap )
  {
    getShape( b0w )->setBoxShape();
    getShape( b1w )->setBoxShape();
  }

  ~VoxmapsAsBoxes()
  {
    getShape( b0w )->setCustomShape();
    getShape( b1w )->setCustomShape();
  }

private:
  static VoxmapShape * getShape( const btCollisionObjectWrapper * bodyWrap )
  {
    return const_cast< VoxmapShape * >
      (static_cast< const VoxmapShape* >(bodyWrap->getCollisionShape()));
  }

  const btCollisionObjectWrapper * b0w, * b1w;
};

class VVCollisionAlgorithm: public btActivatingCollisionAlgorithm
{
  bool m_ownManifold;
  btPersistentManifold * m_manifoldPtr;

public:
  VVCollisionAlgorithm( const btCollisionAlgorithmConstructionInfo& ci )
    : btActivatingCollisionAlgorithm( ci )
  {}

  VVCollisionAlgorithm( btPersistentManifold * mf,
                        const btCollisionAlgorithmConstructionInfo & ci,
                        const btCollisionObjectWrapper * body0Wrap,
                        const btCollisionObjectWrapper * body1Wrap )
    : btActivatingCollisionAlgorithm( ci, body0Wrap, body1Wrap ),
      m_ownManifold( false ),
      m_manifoldPtr( mf )
  {
    if( !m_manifoldPtr &&
        m_dispatcher->needsCollision( body0Wrap->getCollisionObject(),
                                      body1Wrap->getCollisionObject() ) )
      {
        m_manifoldPtr =
          m_dispatcher->getNewManifold( body0Wrap->getCollisionObject(),
                                        body1Wrap->getCollisionObject() );
        m_ownManifold = true;
      }
  }

  virtual ~VVCollisionAlgorithm()
  {
    if( m_ownManifold && m_manifoldPtr )
      m_dispatcher->releaseManifold( m_manifoldPtr );
  }

  virtual void processCollision( const btCollisionObjectWrapper * body0Wrap,
                                 const btCollisionObjectWrapper * body1Wrap,
                                 const btDispatcherInfo& dispatchInfo,
                                 btManifoldResult* resultOut );

  virtual btScalar calculateTimeOfImpact( btCollisionObject * body0,
                                          btCollisionObject * body1,
                                          const btDispatcherInfo & dispatchInfo,
                                          btManifoldResult * resultOut )
  {
    return 1.f;
  }

  virtual void getAllContactManifolds( btManifoldArray & array )
  {
    if( m_manifoldPtr && m_ownManifold )
      array.push_back( m_manifoldPtr );
  }

  struct CreateFunc :public btCollisionAlgorithmCreateFunc
  {
    virtual btCollisionAlgorithm *
    CreateCollisionAlgorithm( btCollisionAlgorithmConstructionInfo & ci,
                              const btCollisionObjectWrapper * body0Wrap,
                              const btCollisionObjectWrapper * body1Wrap )
    {
      int vvsize = sizeof( VVCollisionAlgorithm );
      void * ptr = ci.m_dispatcher1->allocateCollisionAlgorithm( vvsize );
      return new(ptr) VVCollisionAlgorithm( 0, ci, body0Wrap, body1Wrap );
    }
  };
};

struct physics
{
  physics(): dispatcher( &config ),
             world( &dispatcher,
                    &broadphase,
                    &solver,
                    &config ),
             vvcf( NULL )
  {
    void * mem = btAlignedAlloc( sizeof(VVCollisionAlgorithm::CreateFunc), 16 );
    vvcf = new(mem) VVCollisionAlgorithm::CreateFunc;

    dispatcher.registerCollisionCreateFunc( CUSTOM_POLYHEDRAL_SHAPE_TYPE,
                                            CUSTOM_POLYHEDRAL_SHAPE_TYPE,
                                            vvcf );
  }

  ~physics()
  {
    if( vvcf )
      {
	vvcf->~btCollisionAlgorithmCreateFunc();
	btAlignedFree( vvcf );
      }
  }

  btDefaultCollisionConfiguration     config;
  btCollisionDispatcher               dispatcher;
  btDbvtBroadphase                    broadphase;
  btSequentialImpulseConstraintSolver solver;
  btDiscreteDynamicsWorld             world;
  btCollisionAlgorithmCreateFunc *    vvcf;
};

class motion_state: public btMotionState
{
public:
  motion_state( const btTransform & trans ): transform( trans ) {}

  void getWorldTransform( btTransform & trans ) const
  {
    trans = transform;
  }

  void setWorldTransform( const btTransform & trans )
  {
    transform = trans;
  }

  btTransform transform;
};

struct rigid_body
{
  rigid_body( const btVector3 & half_extents,
              const btTransform & trans,
              btScalar mass,
              voxel ** tiles )
    : shape( half_extents, tiles )
    , mstate( trans )
    , info( mass, &mstate, &shape,
            (shape.calculateLocalInertia( mass, inertia ), inertia) )
    , body( info )
  {}

  VoxmapShape shape;
  motion_state mstate;
  btVector3 inertia;
  btRigidBody::btRigidBodyConstructionInfo info;
  btRigidBody body;
};

void VVCollisionAlgorithm::processCollision
( const btCollisionObjectWrapper * body0Wrap,
  const btCollisionObjectWrapper * body1Wrap,
  const btDispatcherInfo& dispatchInfo,
  btManifoldResult* resultOut )
{
  // First, check collision between voxmaps bounding boxes
  {
    VoxmapsAsBoxes vb( body0Wrap, body1Wrap );
    btCollisionAlgorithm * a =
      m_dispatcher->findAlgorithm( body0Wrap, body1Wrap );

    if( a )
      a->processCollision( body0Wrap,
                           body1Wrap,
                           dispatchInfo,
                           resultOut );
  }

  // If voxmaps bounding boxes are overlap, check voxel-wise
  // overlapping
  // FIXME Probably incrementing is a better idea, becase contact
  // points may be sorted.
  const btPersistentManifold * pm = resultOut->getPersistentManifold();
  for( int i = pm->getNumContacts() - 1;
       i >= 0; --i )
    {
      const btManifoldPoint & cp = pm->getContactPoint( i );

      // Find tiles for both voxmaps

      std::cout
        << "contact " << i << "/" << pm->getNumContacts()
        << ", localPoint(A, B) => (" << cp.m_localPointA.x() << " " << cp.m_localPointA.y() << " " << cp.m_localPointA.z() << "), ("
        << cp.m_localPointB.x() << " " << cp.m_localPointB.y() << " " << cp.m_localPointB.z() << ")"
        << ", positionWorldOn(A, B) => (" << cp.getPositionWorldOnA().x() << " " << cp.getPositionWorldOnA().y() << " " << cp.getPositionWorldOnA().z() << "), ("
        << cp.getPositionWorldOnB().x() << " " << cp.getPositionWorldOnB().y() << " " << cp.getPositionWorldOnB().z() << "), "
        << "distance => " << cp.getDistance() << std::endl;
    }
}

extern "C"
{
  physics * init_physics()
  {
    physics * phys = new physics;
    if( phys )
      phys->world.setGravity( btVector3( 0, 0, -10 ) );

    return phys;
  }

  void free_physics( physics * phys )
  {
    delete phys;
  }

  void get_gravity( physics  * phys,
                    btScalar * gravity_x,
                    btScalar * gravity_y,
                    btScalar * gravity_z )
  {
    btVector3 v = phys->world.getGravity();

    *gravity_x = v[ 0 ];
    *gravity_y = v[ 1 ];
    *gravity_z = v[ 2 ];
  }

  void set_gravity( physics  * phys,
                    btScalar   gravity_x,
                    btScalar   gravity_y,
                    btScalar   gravity_z )
  {
    phys->world.setGravity( btVector3( gravity_x, gravity_y, gravity_z ) );
  }

  void step_simulation( physics * phys, btScalar step )
  {
    phys->world.stepSimulation( step );
  }

  void add_body_to_physics( physics * phys,
                            rigid_body * body )
  {
    phys->world.addRigidBody( &body->body );
  }

  void remove_body_from_physics( physics * phys,
                                 rigid_body * body )
  {
    phys->world.removeRigidBody( &body->body );
  }

  rigid_body * make_body( btScalar extent_x,
                          btScalar extent_y,
                          btScalar extent_z,
                          btScalar trans_xx,
                          btScalar trans_xy,
                          btScalar trans_xz,
                          btScalar trans_yx,
                          btScalar trans_yy,
                          btScalar trans_yz,
                          btScalar trans_zx,
                          btScalar trans_zy,
                          btScalar trans_zz,
                          btScalar trans_x,
                          btScalar trans_y,
                          btScalar trans_z,
                          btScalar mass,
                          voxel ** tiles )
  {
    // TODO Reuse box shapes with same half extents
    return new rigid_body( btVector3( extent_x / btScalar( 2. ),
                                      extent_y / btScalar( 2. ),
                                      extent_z / btScalar( 2. ) ),
                           btTransform( btMatrix3x3 ( trans_xx,
                                                      trans_xy,
                                                      trans_xz,
                                                      trans_yx,
                                                      trans_yy,
                                                      trans_yz,
                                                      trans_zx,
                                                      trans_zy,
                                                      trans_zz ),
                                        btVector3( trans_x,
                                                   trans_y,
                                                   trans_z ) ),
                           mass,
                           tiles );
  }

  void delete_body( physics * phys, rigid_body * body )
  {
    if( phys )
      remove_body_from_physics( phys, body );

    delete body;
  }

#define DEF_GET_TRANSFORM( NAME, TRANSFORM )            \
  void get_body_ ## NAME( rigid_body * body,            \
                          btScalar   * trans_xx,        \
                          btScalar   * trans_xy,        \
                          btScalar   * trans_xz,        \
                          btScalar   * trans_yx,        \
                          btScalar   * trans_yy,        \
                          btScalar   * trans_yz,        \
                          btScalar   * trans_zx,        \
                          btScalar   * trans_zy,        \
                          btScalar   * trans_zz,        \
                          btScalar   * trans_x,         \
                          btScalar   * trans_y,         \
                          btScalar   * trans_z )        \
  {                                                     \
    btTransform t = body->mstate.TRANSFORM;             \
    btMatrix3x3 m = t.getBasis();                       \
    *trans_xx = m[ 0 ][ 0 ];                            \
    *trans_xy = m[ 0 ][ 1 ];                            \
    *trans_xz = m[ 0 ][ 2 ];                            \
    *trans_yx = m[ 1 ][ 0 ];                            \
    *trans_yy = m[ 1 ][ 1 ];                            \
    *trans_yz = m[ 1 ][ 2 ];                            \
    *trans_zx = m[ 2 ][ 0 ];                            \
    *trans_zy = m[ 2 ][ 1 ];                            \
    *trans_zz = m[ 2 ][ 2 ];                            \
                                                        \
    btVector3 v = t.getOrigin();                        \
    *trans_x = v[ 0 ];                                  \
    *trans_y = v[ 1 ];                                  \
    *trans_z = v[ 2 ];                                  \
  }

  DEF_GET_TRANSFORM( transform, transform );
  DEF_GET_TRANSFORM( itransform, transform.inverse() );

#undef DEF_GET_TRANSFORM

void set_body_transform( rigid_body * body,
                         btScalar     trans_xx,
                         btScalar     trans_xy,
                         btScalar     trans_xz,
                         btScalar     trans_yx,
                         btScalar     trans_yy,
                         btScalar     trans_yz,
                         btScalar     trans_zx,
                         btScalar     trans_zy,
                         btScalar     trans_zz,
                         btScalar     trans_x,
                         btScalar     trans_y,
                         btScalar     trans_z )
{
  body->mstate.transform = btTransform( btMatrix3x3 ( trans_xx,
                                                      trans_xy,
                                                      trans_xz,
                                                      trans_yx,
                                                      trans_yy,
                                                      trans_yz,
                                                      trans_zx,
                                                      trans_zy,
                                                      trans_zz ),
                                        btVector3( trans_x,
                                                   trans_y,
                                                   trans_z ) );
}

}
