Description
===========

Cleven is an experimental game engine with volume graphics written in
Common Lisp.

Volume rendering is a very attractive technique for displaying fire,
smoke, clouds and other semi-transparent objects.  Volume objects can
have very complex internal structures.  Creation, modification and
editing of such objects can be quite simple and effective.  In fact,
the majority of 3D editing operations are similar to editing
operations in 2D raster graphics.

Cleven's main goal is to explore new design approaches to video games
and interactive visualization basing on the still unharnessed
potential of volume graphics.

Cleven has nothing else but volume objects.  However, this limitation
helps drastically simplify its implementation: there is no mixed
polygon-volume rendering, we deal with only one data format and so on.

So far, it has a simple slice-based OpenGL renderer with an
orthographic projection, basic support of volume sprites and a simple
converter from polygonal models to voxel maps.

Current status
--------------

Please note that Cleven is at an early stage of development.  Many parts of
the project have no optimization, are very slow and have rather simple,
<del>stupid</del> straightforward solutions.

At present the following has been implemented:

 - Simple tiled renderer with orthographic projection.
 - Loading of volumes in the form of voxel maps: tiled raw raster
   data and a separate header file.
 - Basic support of volume sprites: adding to the game world, moving,
   rotation.
 - Converter from a polygonal model to a voxel map.  Conversion
   is performed only for the geometry of the polygonal model.  No textures or
   colors of vertexes/polygons are used.

Cleven was tested on SBCL and GNU/Linux.  Memory mapped voxel maps on
Windows are not supported, but this support is planned.

Other issues and limitations are:

 - Rendering defects on boundaries of sprites tiles.
 - Implemented is only one light source: the sun.
 - Implemented is only one camera.
 - Many others. ;-)

Build and run
-------------

Install [SDL 2.0](http://www.libsdl.org/),
[SBCL](http://www.sbcl.org/) and
[Quicklisp](http://www.quicklisp.org/).

Run SBCL and install Common Lisp bindings for SDL 2.0 using Quicklisp.

```Lisp
(ql:quickload "sdl2")
```

Load the Cleven system.

```Lisp
(asdf:load-system :cleven)
```

Load the file sandbox.lisp, change the current package and run the
Cleven sandbox.

```Lisp
(load "sandbox.lisp")
(in-package #:sandbox)
(run-sandbox)
```

The default key binding in the sandbox:

 - Left, right: changes the camera azimuth, i.e. the camera is rotated
   in the horizontal plane and around the point ```*camera-location*```.
 - Up, down: changes the camera tilt, i.e. the camera is rotated in
   the vertical plane and around the point ```*camera-location*```.
 - +, -: zoom in and zoom out.
 - q: quit the sandbox.

Make the game world and add some sprites.

```Lisp
(make-global-world 256 256 256)
(defparameter *sprite* (make-sprite :sprite "/path/to/sprite"))
(defparameter *sprite2* (make-sprite :sprite2 "/path/to/sprite2"))
(wob :add-to-world *sprite* (locat 100 110 120))
(wob :add-to-world *sprite2* (locat 90 80 70))
```

See _suzanne.lisp_ in the example directory.

To convert polygonal models to voxmaps install
[MeshLab](http://www.meshlab.org/),
[Teem](http://teem.sourceforge.net/).  Conversion is preformed using
io_voxels plugin for MeshLab.  To build it you should also install
[VCG Library](http://vcg.sourceforge.net/), MeshLab source code and
[Qt](https://www.qt.io/).

Install MeshLab and VCG Library source code in the parent directory of
Cleven repository.  If you install them in other places you should
change paths in the file _io_voxels.pro_:

```
include (../../meshlab/src/shared.pri)
INCLUDEPATH += ../../meshlab/src ../../vcglib
LIBPATH += ../../meshlab/src/distrib
DESTDIR = ../../meshlab/src/distrib/plugins

```

Then build io_voxels plugin.


```
cd /path/to/cleven/io_voxels
qmake
make
```

Resampling of voxel data is performed during conversion using _unu_
command-line tool from Teem.  So make sure that it is in your search
path (check PATH environment variable).

Run conversion using _meshlabserver_.

```
meshlabserver -i model.ply -o model.voxels
```

Conversion generates two files: _model_ and _model.voxels_.  The former
file is a voxmap header, the latter is voxel data.

Road Map
--------

 - Physics simulation using Bullet Physics Library.
 - Volumetric boolean operations and real-time volume data processing.
 - Visual effects: fire, smoke, particles.
 - Fracture simulation.
 - High-level game-oriented scripting language.
 - Multiplayer support.

Sample image
------------

![Suzanne](https://github.com/afainer/cleven/blob/master/suzanne.png)
