;;; Copyright (c) 2015, Andrey Fainer <fandrey@gmx.com>
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
    (dobox (l (locat- loc +voxmap-tile-size+)
              (locat+ loc +voxmap-tile-size+)
              +voxmap-tile-size+)
      (push l lst))
    lst))

(defun sprite-transform (loc sloc sorig srotmat)
  "Transform LOC using the sprite location, origin and rotation.
The sprite location, origin and rotation matrix are SLOC, SORIG,
SROTMAT respectively."
  (transform (mat44mul (translation-matrix-loc sloc)
                       srotmat
                       (translation-matrix-loc (locat- sorig)))
             loc))

(defmacro do-sprite-tiles ((tiles &rest locations) &body body)
  "Evaluate forms of BODY for each tile of TILES.
LOCATIONS is list of variable names which are bound to the sprite
locations.  For each name create variable <NAME>-locs which is bound
to the current tile location and its eight corners."
  (with-gensyms (gtiles gtile gloc)
    `(let ((,gtiles ,tiles))
       (dolist (,gtile ,gtiles)
         (destructuring-bind (tile . ,gloc) ,gtile
           (let (,@(mapcar
                    #'(lambda (loc)
                        `(,(symbolicate loc '-locs)
                           (sprite-tile-locs
                            (sprite-transform (locat+ ,gloc
                                                      (/ +voxmap-tile-size+ 2))
                                              ,loc
                                              origin
                                              rotmat))))
                    locations))
             ,@body))))))

;;; FIXME Too many arguments
(defun sprite-move (newloc delta location origin rotmat tiles)
  "Move sprite from LOCATION to NEWLOC.
If DELTA is non-nil, then NEWLOC is added to LOCATION.  ORIGIN, ROTMAT
and TILES are the sprite origin, rotation matrix and tiles
respectively."
  (let ((old (copy-locat location)))
    (setq location (if delta (locat+ location newloc) newloc))
    (unless (locat= old location)
      (do-sprite-tiles (tiles old location)
        (move-wob tile old-locs location-locs)))
    location))

(defun make-sprite (name voxmap-file)
  "Make a sprite named NAME and load VOXMAP-FILE for its voxels.
For each tile in the sprite voxmap a wob is created.  Return the last
wob.  The sprite wob has the following operations.

:VOXELS -- voxels of the tile for which the wob is created.

:LOCATION -- the current location of the sprite.

:IROTMAT -- the inverted rotation matrix of the sprite.

:ORIGIN -- the origin of the sprite; at the moment the origin is the
center of the sprite voxmap.

:MOVE, :MV -- move the sprite in `*world*'; arguments are a new
location and optional delta, non-nil value of which specify: new-loc
== (+ old-loc location).

:ROTATE, :ROT -- rotate the sprite around the sprite origin; arguments
are the rotation axis and an angle in degrees.

:ADD-TO-WORLD -- put the sprite to `*world*'; the operation argument
is a location.

:RM-FROM-WORLD -- remove the sprite from `*world*'."
  (let* ((voxmap (load-voxmap voxmap-file t))
         (finalizer #'(lambda () (free-voxmap voxmap nil)))
         (location (locat))
         (origin (locat/ (voxmap-size voxmap) 2))
         (rotmat (identity-matrix))
         (irotmat (identity-matrix))    ;inverted rotation
         (tiles)
         (mv #'(lambda (loc &optional delta)
                 (setq location
                       (sprite-move loc delta location
                                    origin rotmat tiles))))
         (rot #'(lambda (axis angle &optional delta)
                  ;; FIXME Make a new rotation matrix if DELTA is nil.
                  (setq rotmat
                        (mat44mul (rotation-matrix axis angle)
                                  rotmat)
                        irotmat
                        (mat44mul irotmat
                                  (rotation-matrix axis (- angle)))))))
    (dovoxmap voxmap
      (push (cons
             (make-wob
              name
              ;; FIXME Voxels, location, (i)rotmat, origin should be
              ;; accessed with held lock
              :voxels #'(lambda () tile)
              :location #'(lambda ()
                            location)
              :irotmat #'(lambda () irotmat)
              :origin #'(lambda () (locat- origin tileloc))
              :move mv
              :rotate rot
              :mv mv
              :rot rot
              :add-to-world #'(lambda (loc)
                              (setq location loc)
                              (do-sprite-tiles (tiles location)
                                (dolist (loc location-locs)
                                  (add-to-world tile loc))))
              :rm-from-world #'(lambda ()
                               (do-sprite-tiles (tiles location)
                                 (dolist (loc location-locs)
                                   (remove-from-world tile loc)))))
             tileloc)
            tiles))
    (trivial-garbage:finalize tiles finalizer)
    (caar tiles)))
