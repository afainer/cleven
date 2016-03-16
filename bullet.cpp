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

#include <btBulletDynamicsCommon.h>

struct physics
{
  physics(): dispatcher( &config ),
             world( &dispatcher,
                    &broadphase,
                    &solver,
                    &config )
  {}

  btDefaultCollisionConfiguration     config;
  btCollisionDispatcher               dispatcher;
  btDbvtBroadphase                    broadphase;
  btSequentialImpulseConstraintSolver solver;
  btDiscreteDynamicsWorld             world;
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
              btScalar mass )
    : shape( half_extents )
    , mstate( trans )
    , info( mass, &mstate, &shape,
            (shape.calculateLocalInertia( mass, inertia ), inertia) )
    , body( info )
  {}

  btBoxShape shape;
  motion_state mstate;
  btVector3 inertia;
  btRigidBody::btRigidBodyConstructionInfo info;
  btRigidBody body;
};

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
                          btScalar mass )
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
                           mass );
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
