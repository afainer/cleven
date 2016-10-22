;;; Copyright (c) 2015-2016, Andrey Fainer <fandrey@gmx.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package #:cleven)

(defun sprite-tile-locs (loc)
  "Center at LOC and eight corners of a sprite tile."
  (let ((lst))
    (dobox (l (vec- loc (vecn +voxmap-tile-size+))
              (vec+ loc (vecn +voxmap-tile-size+))
              +voxmap-tile-size+)
      (push l lst))
    lst))

(macrolet ((defsprite-data (&rest vars)
             `(progn
                (defun sprite-data ,vars
                  (list ,@vars))
                (defmacro with-sprite-data (sd &body body)
                  `(destructuring-bind ,',vars ,sd
                     (declare (ignorable ,@',vars))
                     ,@body)))))
  (defsprite-data location origin rotmat))

(defun transform-sprite-point (loc data)
  "Transform LOC using sprite DATA (location, origin, rotation)."
  (with-sprite-data data
    (transform-point loc
                     (matrix* (translate location)
                              rotmat
                              (translate (vec* origin -1f0))))))

(defun sprite-bbox (data size)
  "Sprite bounding box with transformations."
  (letvec ((( x  y  z) (apply #'vec size)))
    ;; The order is matter
    (list (transform-sprite-point (vec 0 0 0) data)
          (transform-sprite-point (vec x 0 0) data)
          (transform-sprite-point (vec 0 y 0) data)
          (transform-sprite-point (vec x y 0) data)
          (transform-sprite-point (vec 0 0 z) data)
          (transform-sprite-point (vec x 0 z) data)
          (transform-sprite-point (vec 0 y z) data)
          (transform-sprite-point (vec x y z) data))))

(defun make-sprite (name voxmap-file)
  "Make a sprite named NAME and load VOXMAP-FILE for its voxels.
For each tile in the sprite voxmap a wob is created.  Return the last
wob.  The sprite wob has the following operations.

:VOXELS -- voxels of the tile for which the wob is created.

:SIZE -- size of the sprite voxmap.

:LOCATION -- the current location of the sprite.

:IROTMAT -- the inverted rotation matrix of the sprite.

:ORIGIN -- the origin of the sprite; at the moment the origin is the
center of the sprite voxmap.

:MOVE, :MV -- move the sprite in `*world*'; arguments are a new
location and optional delta, non-nil value of which specify: new-loc
== (+ old-loc location).

:ROTATE, :ROT -- rotate the sprite around the sprite origin; arguments
are the rotation axis and an angle in degrees.

:TRANSFORM -- transform the sprite in the `*world*'; arguments are a
translation vec and a rotation matrix."
  (let* ((voxmap (load-voxmap voxmap-file)) ; TODO Use mmap for big sprites
         (finalizer #'(lambda () (free-voxmap voxmap nil)))
         (location (vec))
         (origin (vec/ (apply #'vec (voxmap-size voxmap)) 2))
         (rotmat (identity-matrix))
         (irotmat (identity-matrix))    ;inverted rotation
         sprite)
    (flet ((sprite-data ()
             (sprite-data location origin rotmat)))
      (declare (inline sprite-data))
      (let ((mv #'(lambda (loc &optional delta)
                    (setq location (if delta (vec+ location loc) loc))))
            (rot #'(lambda (axis angle &optional delta)
                     (let ((v (cond ((or (eq axis :x) (= axis 0))
                                     (vec angle 0 0))
                                    ((or (eq axis :y) (= axis 1))
                                     (vec 0 angle 0))
                                    ((or (eq axis :z) (= axis 2))
                                     (vec 0 0 angle)))))
                       (iflet delta
                           ((newrot (matrix* (rotated v) rotmat)
                                    (rotated v))
                            (newirot (matrix* (rotated (vec* v -1f0)) irotmat)
                                     (rotated (vec* v -1f0))))
                         (setq rotmat newrot
                               irotmat newirot))))))
        (setq sprite
              (make-wob
               name
               ;; FIXME Voxels, location, (i)rotmat, origin should be
               ;; accessed with held lock
               :voxmap #'(lambda () voxmap)
               :size #'(lambda () (voxmap-size voxmap))
               :location #'(lambda () location)
               :irotmat #'(lambda () irotmat)
               :origin #'(lambda () origin)
               :move mv
               :rotate rot
               :mv mv
               :rot rot
               :transform #'(lambda (rotation translation irotation)
                              (setq rotmat rotation
                                    irotmat irotation
                                    location translation))
               :bbox #'(lambda ()
                         (sprite-bbox (sprite-data) (voxmap-size voxmap)))))))
    (trivial-garbage:finalize sprite finalizer)
    sprite))
