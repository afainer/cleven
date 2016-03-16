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

(defvar *physics* nil
  "Discrete dynamics world.  See `init-physics', `free-physics'.")

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
  (with-foreign-vecs (v)
    (cfuncall "get_gravity" :pointers (*physics* v))
    v))

(defun (setf physics-gravity) (new)
  "Set physics gravity"
  (cfuncall "set_gravity"
            :pointer *physics*
            vec new)
  new)

(defun step-simulation (step)
  "Proceeds the physics simulation over STEP seconds."
  (cfuncall "step_simulation"
            :pointer *physics*
            scalar step))

(defun add-body-to-physics (body)
  "Add BODY to the dynamics world."
  (cfuncall "add_body_to_physics"
            :pointers (*physics* body))
  body)

(defun remove-body-from-physics (body)
  "Add BODY from the dynamics world."
  (cfuncall "remove_body_from_physics"
            :pointers (*physics* body)))

(defun make-body (size rotation translation mass)
  "Make body with box SIZE and MASS.
The initial world transform is ROTATION and TRANSLATION."
  (cfuncall "make_body"
            vec size
            mat3 rotation
            vec translation
            scalar mass
            :pointer))

(defun delete-body (body)
  "Remove BODY from the dynamic world and delete it."
  (cfuncall "delete_body"
            :pointers ((if *physics*
                           *physics*
                           (cffi:null-pointer))
                       body)))

(defun get-body-transform (body)
  "Get BODY world transform.
Return two values: rotation and translation."
  (with-foreign-mat3s (m)
    (with-foreign-vecs (v)
      (cfuncall "get_body_transform"
                :pointers (body m v))
      (values m v))))

(defun set-body-transform (body rotation translation)
  "Set BODY world transform with ROTATION ans TRANSLATION."
  (cfuncall "set_body_transform"
            :pointer body
            mat3 rotation
            vec translation))

(defun get-body-itransform (body)
  "Get BODY inverted world transform.
Return two values: rotation and translation."
  (with-foreign-mat3s (m)
    (with-foreign-vecs (v)
      (cfuncall "get_body_itransform"
                :pointers (body m v))
      (values m v))))

(defvar *pobs-in-world* nil
  "Physics objects in `*world'.")

(defmacro flet-pob (bodiesvar addbody rmbody updatebody &body forms)
  "A helper macro which generate `flet' for `make-pob'."
  (let ((addbodyfn '#'(lambda (translation rotation)
                        (unless bodyptr
                          ;; TODO :add-to-world of a sprite should
                          ;; also take a rotation argument.
                          (wob :add-to-world sprite translation)
                          (setq bodyptr
                                (make-body (apply #'vec (wob :size sprite))
                                           rotation
                                           translation
                                           mass))
                          (add-body-to-physics bodyptr))))
        (rmbodyfn '#'(lambda ()
                       (when bodyptr
                         (wob :rm-from-world sprite)
                         (delete-body bodyptr)
                         (setq bodyptr nil))))
        (updatebodyfn '#'(lambda ()
                           (multiple-value-bind (rot tr)
                               (get-body-transform bodyptr)
                             (wob :transform sprite
                                  rot tr
                                  ;; Sprites don't use inverted
                                  ;; translations
                                  (get-body-itransform bodyptr))))))
    `(flet
         ((,addbody (body-data loc rotation)
            (funcall (cadr body-data) loc rotation))
          (,rmbody (body-data)
            (funcall (caddr body-data)))
          (,updatebody (body-data)
            (funcall (cadddr body-data)))
          (body (name &key sprite mass
                      (translation (vec)) (rotation (identity-matrix)))
            "Define a rigid body with name NAME."
            (unless sprite
              (error "A body definition must have the sprite argument."))
            (unless mass
              (error "A body definition must have the mass argument."))
            (let* ((sprite (if (or (pathnamep sprite) (stringp sprite))
                               (make-sprite name sprite)
                               sprite))
                   (bodyptr)
                   (body-data (list name ,addbodyfn ,rmbodyfn ,updatebodyfn)))
              (push body-data ,bodiesvar)
              (trivial-garbage:finalize body-data ,rmbodyfn))))
       (declare (inline ,addbody ,rmbody ,updatebody))
       ,@forms)))

(defmacro make-pob (name &body forms)
  "Make a physics object with the name NAME.
A physics object is a WOB with the following operations:

:ADD-TO-WORLD -- add the object to `*world*';  arguments are a
location and optional rotation.

:RM-FROM-WORLD -- remove the object from `*world'.

:UPDATE -- update transform of the object's sprites; the operation is
used in `do-physics-simulation'.

FORMS is a list of Lisp expressions which must define at least one
rigid body of the created object.  In scope of FORMS the local
function `body' is defined:

  body name &key sprite mass translation rotation

NAME -- the name of the defined body.

SPRITE -- the sprite of the body; SPRITE is required argument; if
SPRITE is a string of a pathname then the sprite is loaded when the
body is created.  Otherwise the value of SPRITE must be a WOB created
with `make-sprite'.

MASS -- the mass of the body; the argument is required.

TRANSLATION -- the initial translation of the body relative to its POB
origin.

ROTATION -- the initial rotation of the body."
  (with-gensyms (bodies addbody rmbody updatebody pob)
    `(let (,bodies ,pob)
       (flet-pob ,bodies ,addbody ,rmbody ,updatebody
         ,@forms
         (unless ,bodies
           (error "No rigid bodies.  At least one body must be defined."))
         (setq ,pob
               (make-wob
                ,name
                :add-to-world #'(lambda (loc &optional (rot (identity-matrix)))
                                  (dolist (b ,bodies)
                                    ;; FIXME Do not place all sprites
                                    ;; at the same location.
                                    (,addbody b loc rot))
                                  ;; TODO Lock
                                  (pushnew ,pob *pobs-in-world*))
                :rm-from-world #'(lambda ()
                                   (dolist (b ,bodies)
                                     (,rmbody b))
                                   ;; TODO Lock
                                   (setq *pobs-in-world*
                                         (delete ,pob *pobs-in-world*)))
                :update #'(lambda ()
                            (dolist (b ,bodies)
                              (,updatebody b)))))))))

;;; For simplicity the physics simulation is performed in the render thread.
(let (time)
  (defun do-physics-simulation ()
    "Step the physics simulation."
    (let ((newtime (get-internal-real-time)))
      (when *pobs-in-world*
        (step-simulation (/ (abs (- newtime time))
                            internal-time-units-per-second))
        (dolist (pob *pobs-in-world*)
          (wob :update pob)))
      (setq time newtime)))

  (defun start-physics-simulation ()
    "Start the physics simulation."
    (setq time (get-internal-real-time))
    (lock #'add-hook *render-loop-hook-lock* 'do-physics-simulation))

  (defun stop-physics-simulation ()
    "Stop the physics simulation."
    (lock #'remove-hook *render-loop-hook-lock* 'do-physics-simulation)
    (setq time nil)))
