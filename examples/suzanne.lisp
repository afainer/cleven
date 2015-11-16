;;; To load this file the Lisp process current directly should be
;;; the parent of the examples directory.

(make-global-world 256 256 256)

;;; To make sprites from example voxmaps unzip them from suzanne.zip.
;;; Then use headers (files without suffix ".voxels") as the second
;;; argument of `make-sprite'.

;;; The first argument is intended to be used as a sprite's unique
;;; identifier, but at present is used only for debugging.
(defparameter *suzanne* (make-sprite :suzanne "examples/suzanne-white"))
(defparameter *suzanne-red* (make-sprite :suzanne-red "examples/suzanne-red"))
(defparameter *suzanne-green* (make-sprite :suzanne-green "examples/suzanne-green"))
(defparameter *suzanne-blue* (make-sprite :suzanne-blue "examples/suzanne-blue"))

;;; `make-sprite' returns a world object or WOB.  The function `wob'
;;; applies the operation (method) of an object on its arguments.  The
;;; first argument of `wob' is the operation name and the second one
;;; is the object.  The rest of arguments is the operation arguments.

;;; Put a sprite into the game world at specified location
(wob :add-to-world *suzanne* (locat 63 64 64))
(wob :add-to-world *suzanne-red* (locat 63 160 196))
(wob :add-to-world *suzanne-green* (locat 170 160 196))
(wob :add-to-world *suzanne-blue* (locat 170 64 64))

;;; Remove a sprite from the game world
;;; (wob :rm-from-world *suzanne*)

;;; Move to the absolute location in the game world
;;; (wob :mv *suzanne* (locat 10 20 30))
;;;
;;; Move to the location relative to the current one
;;; (wob :mv *suzanne* (locat 1 2 3) t)

;;; Rotations from each axis.  The rotation angle is in degrees.
;;; (wob :rot *suzanne* :x  10)
;;; (wob :rot *suzanne* :y -10)
;;; (wob :rot *suzanne* :z  20)

(setq *camera-azimuth* 20
      *camera-tilt* 60
      *camera-location* (locatn 128))
