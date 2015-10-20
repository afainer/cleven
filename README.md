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

Cleven has nothing else but volume objects.  However, this limitation
helps drastically simplify its implementation: there is no mixed
polygon-volume rendering, we deal with only one data format and so on.

For volume rendering a simple slice-based OpenGL renderer is
implemented.  So far, the renderer has only orthographic projection,
therefore Cleven is intended to be used for games with isometric
graphics.

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

Cleven was tested on SBCL and GNU/Linux.  It will not work on Windows,
because voxel maps are loaded using memory mapping, which is not
supported on Windows, but this support is planned.

Other issues and limitations are:

 - Too few sprites per rendering tile.
 - Rendering defects on boundaries of sprites tiles.
 - Implemented is only one light source: the sun.
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
   in the horizontal plane and around the point `*camera-location*`.
 - Up, down: changes the camera tilt, i.e. the camera is rotated in
   the vertical plane and around the point `*camera-location*`.
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

To convert polygonal models to voxmaps install
[MeshLab](http://www.meshlab.org/),
[Teem](http://teem.sourceforge.net/) and [Qt](https://www.qt.io/).

Build io_voxels plugin for MeshLab.

```
cd /path/to/cleven/io_voxels
qmake
make
```

Run conversion using `meshlabserver'.

```
meshlabserver -i model.ply -o model.voxels
```

Road Map
--------

 - Physics simulation using Bullet Physics Library.
 - Volumetric boolean operations and real-time volume data processing.
 - Visual effects: fire, smoke, particles.
 - Fracture simulation.
 - High-level game-oriented scripting language.

Sample image
------------

![Suzanne](https://github.com/afainer/cleven/blob/master/suzanne.png)
