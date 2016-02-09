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

;;; For simplicity the physics simulation is performed in the render thread.

(defvar *physics* nil
  "TODO Doc")

(define-foreign-library libphysics
  ;; FIXME Search path
  (t (:default "./libbullet")))

(use-foreign-library libphysics)

(defun init-physics ()
  "Create discrete dynamics world."
  (if *physics*
      (free-physics))
  (aif* (foreign-funcall "init_physics" :pointer)
        (not (null-pointer-p it))
        (setq *physics* it)))

(defun free-physics ()
  "Delete discrete dynamics world."
  (foreign-funcall "free_physics" :pointer *physics*)
  (setq *physics* nil))

(defun physics-gravity ()
  "Get physics gravity"
  (with-foreign-locats (loc)
    (cfuncall "get_gravity" :pointers (*physics* loc))
    loc))

(defun (setf physics-gravity) (new)
  "Set physics gravity"
  (cfuncall "set_gravity"
            :pointer *physics*
            locat new)
  new)
