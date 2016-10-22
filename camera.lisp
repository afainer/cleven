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

(defvar *camera-zoom* 1.0)

(defvar *camera-tilt* 45)

(defvar *camera-azimuth* 45)

(defvar *camera-location* (vec)
  "The location of the camera.")

(defun camera-matrix ()
  "The transformation matrix of the camera."
  (matrix* (rotated* (- *camera-tilt*) 0f0 0f0)
           (rotated* 0f0 0f0 *camera-azimuth*)
           (translate (vec* *camera-location* -1f0))))

;;; FIXME Zoom
(defun camera-frustum-size ()
  (/ (apply #'min
            (multiple-value-list
             (sdl2:get-window-size *render-window*)))
     *camera-zoom*))

(defun make-ortho ()
  "Make the ortho projection matrix."
  (let ((s (float (/ 2 (camera-frustum-size)) 0f0)))
    (matrix s   0f0 0f0 0f0
            0f0 s   0f0 0f0
            0f0 0f0 s   0f0
            0f0 0f0 0f0 1f0)))

(defun camera-view ()
  (matrix* (make-ortho) (camera-matrix)))

(defun camera-vector ()
  "The camera look-at vector without the camera location."
  (transform-point (vec 0 0 -1)
                   (matrix* (rotated* 0f0 0f0 (- *camera-azimuth*))
                            (rotated* *camera-tilt* 0f0 0f0))))

(defun camera-sees ()
  "Which sprites the camera sees"
  (labels ((line (x x1 x2 y1 y2)
             (and (or (and (< x1 x) (> x2 x))
                      (and (< x2 x) (> x1 x)))
                  (+ (* (/ (- x x1) (- x2 x1))
                        (- y2 y1))
                     y1)))
           (cr (x x1 x2 y1 y2)
             (let ((y (line x x1 x2 y1 y2)))
               (and y (>= y 0f0) (< y 1f0))))
           (cross (v1 v2)
             (with-vecs (v1 v2)
               (when (or (cr 0f0 v1x v2x v1y v2y)
                         (cr 1f0 v1x v2x v1y v2y)
                         (cr 0f0 v1y v2y v1x v2x)
                         (cr 1f0 v1y v2y v1x v2x))
                 t)))
           (crossf (face)
             (or (cross (nth 0 face) (nth 1 face))
                 (cross (nth 1 face) (nth 2 face))
                 (cross (nth 2 face) (nth 3 face))
                 (cross (nth 3 face) (nth 0 face))))
           (inside (face)
             ;; Check only one point.  If other tests failed then the
             ;; camera screen is entirely either inside or outside FACE.
             (let ((c 0))
               (mapc #'(lambda (v1 v2)
                         (with-vecs (v1 v2)
                           (let ((l (line 0f0 v1y v2y v1x v2x)))
                             (if (and l (> l 0f0)) (incf c)))))
                     (list (nth 0 face) (nth 1 face) (nth 2 face) (nth 3 face))
                     (list (nth 1 face) (nth 2 face) (nth 3 face) (nth 0 face)))
               (= c 1))))
    (let (lst)
      (dolist (o *world* (nreverse lst))
        (aif (wob :bbox o)
             (let* ((m (camera-view))
                    (verts (mapcar #'(lambda (v)
                                       (transform-point v m))
                                   it)))
               (when (or (some #'(lambda (v)
                                   (letvec (((x y z) v))
                                     (and (>= x 0f0) (>= y 0f0)
                                          (<  x 1f0) (<  y 1f0))))
                               verts)
                         (let ((faces (flet ((f (v1 v2 v3 v4)
                                               (list (nth v1 verts)
                                                     (nth v2 verts)
                                                     (nth v3 verts)
                                                     (nth v4 verts))))
                                        (list
                                         (f 0 1 3 2) (f 4 5 7 6)
                                         (f 0 1 5 4) (f 2 3 7 6)
                                         (f 2 0 4 6) (f 3 1 5 7)))))
                           (or (some #'crossf faces)
                               (some #'inside faces))))
                 (push o lst))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-camera-ordering ()
    "Generate ordering for `do-camera-box'."
    (mapcan #'(lambda (l)
                (destructuring-bind (x y z) l
                  (let ((-x (symbolicate '- x))
                        (-y (symbolicate '- y))
                        (-z (symbolicate '- z)))
                    (list (list  x  y  z)
                          (list -x  y  z)
                          (list  x -y  z)
                          (list -x -y  z)
                          (list  x  y -z)
                          (list -x  y -z)
                          (list  x -y -z)
                          (list -x -y -z)))))
            (list '(x y z) '(y x z) '(x z y)
                  '(z x y) '(y z x) '(z y x)))))

(defmacro do-camera-box (varloc &body body)
  "Evaluate forms of BODY for each world tile in the camera frustum.
VARLOC is the current tile location."
  ;; At the moment approximate viewable area with cube which size is
  ;; equal to diagonal of camera frustum.
  (with-gensyms (gloc size)
    `(let* ((,size (/ (camera-frustum-size) 2))
            (orders
             (list
              ,@(let ((ord))
                  (dolist (o (do-camera-ordering) ord)
                    (push `(cons ',o
                                 #'(lambda ()
                                     (let ((*camera-azimuth* *camera-azimuth*)
                                           (*camera-tilt* *camera-tilt*)
                                           (*camera-location* *camera-location*)
                                           (,gloc
                                            (vec* (trunc-vec *camera-location*
                                                             +voxmap-tile-size+)
                                                  +voxmap-tile-size+)))
                                       (dobox
                                           (,varloc
                                            (vec- ,gloc (vecn ,size))
                                            (vec+ ,gloc (vecn ,size))
                                            +voxmap-tile-size+
                                            ,o)
                                         ,@body))))
                          ord))))))
       (setq ,size (* (ceiling (sqrt (* ,size ,size)) +voxmap-tile-size+)
                      +voxmap-tile-size+))
       (letvec (((x y z) (camera-vector)))
         (let ((order (list (cons (abs x) (if (plusp x) '-x 'x))
                            (cons (abs y) (if (plusp y) '-y 'y))
                            (cons (abs z) (if (plusp z) '-z 'z)))))
           (funcall
            (cdr (assoc (mapcar #'cdr
                                (sort order #'> :key #'car))
                        orders
                        :test #'equal))))))))
