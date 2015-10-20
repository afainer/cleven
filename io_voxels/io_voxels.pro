include (/home/alf/src/meshlab/src/shared.pri)

INCLUDEPATH += ../../meshlab/src ../../vcglib
LIBPATH += ../../meshlab/src/distrib
DESTDIR = ../../meshlab/src/distrib/plugins

HEADERS = io_voxels.h

SOURCES = io_voxels.cpp

TARGET = io_voxels
