cmake_minimum_required( VERSION 2.8 )
set( BUILD_SHARED_LIBS true )

find_package( Bullet REQUIRED )
if( BULLET_FOUND )
  include_directories( ${BULLET_INCLUDE_DIRS} )
endif()

add_library( bullet bullet.cpp )
