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

(defvar *render-init-hook* ()
  "Hook run before the rendering is started.")

(defvar *render-init-hook-lock* (make-shared '*render-init-hook*)
  "The shader object for `*render-init-hook*'.")

(defvar *render-loop-hook* ()
  "Hook run for each the rendering loop iteration.")

(defvar *render-loop-hook-lock* (make-shared '*render-loop-hook*)
  "The shader object for `*render-loop-hook*'.")

(defvar *render-quit-hook* ()
  "Hook run after the rendering is ended.")

(defvar *render-quit-hook-lock* (make-shared '*render-quit-hook*)
  "The shader object for `*render-quit-hook*'.")

;;; TODO Move to game.lisp or wrap into a function
(defvar *render-window* nil
  "The window in which the current render thread draws.")

(defvar *render-glc* nil
  "The GL context in which the current render thread draws.")

(defvar *default-vertex-shader* "vertex.glsl"
  "Default vertex shader source filename.")

(defvar *default-fragment-shader* "fragment.glsl"
  "Default fragment shader source filename.")

(defconstant +vertex-attrib-location+ 0)

(defvar *sun-tilt* 45
  "Tilt of the sun.
Is not used at the moment. Hardcoded in the fragment shader.")

(defvar *sun-azimuth* 0
  "Azimuth of the sun.
Is not used at the moment. Hardcoded in the fragment shader.")

(defvar *sun-color* (list 1 1 1)
  "The sun color and intensity.
Is not used at the moment.  Hardcoded in the fragment shader.")

(defvar *ambient-light* (list .1 .1 .1)
  "The ambient light color and intensity.
Is not used at the moment.")

(defun render-loop (win)
  "Run the render loop in a separate thread."
  (make-game-thread ("Render thread")
    (let ((win win))
      (sdl2:with-gl-context (glc win)
        (sdl2:gl-make-current win glc)
        (let ((*render-window* win)
              (*render-glc* glc))
          (gl:disable :dither)
          (gl:enable :alpha-test)
          (gl:alpha-func :greater 0)
          (gl:enable :blend)
          (gl:blend-equation :func-add)
          (gl:blend-func :one :one-minus-src-alpha)
          (unwind-protect
               (progn (lock #'run-hooks *render-init-hook-lock*)
                      (thread-loop (lock #'run-hooks *render-loop-hook-lock*)))
            (lock #'run-hooks *render-quit-hook-lock*)))))))

(macrolet
    ((def-tiles (&rest tilevars)
       `(let ,tilevars
          (defun init-texture-tiles ()
            "Initialize texture size, tiles size and coordinates."
            (setq max-tex-size 256 ;; (gl:get* :max-3d-texture-size)
                  tex-tile-size (/ +voxmap-tile-size+ max-tex-size)
                  tiles-coords nil
                  tex-tiles-coords nil)
            (dobox (coords
                    (vec)
                    (vecn (/ (1- max-tex-size) +voxmap-tile-size+)))
              (let ((c (vec* coords +voxmap-tile-size+)))
                (push c tiles-coords)
                (push (vec/ c max-tex-size) tex-tiles-coords)))
            (setq max-tex-attribs (1- (gl:get* :max-vertex-attribs))
                  tiles-coords (nreverse tiles-coords)
                  tex-tiles-coords (nreverse tex-tiles-coords)))

          ,@(let (fns)
                 (dolist (var tilevars (nreverse fns))
                   (push `(defun ,var () ,var) fns)))

          (defun make-texture ()
            (gl:tex-image-3d :texture-3d 0 :rgba
                             (max-tex-size) (max-tex-size) (max-tex-size)
                             0 :rgba :unsigned-byte
                             (cffi:null-pointer))))))
  (def-tiles
      ;; TODO Redefine as parameters.
      max-tex-size tex-tile-size tex-tiles-coords tiles-coords max-tex-attribs))

(defun tile-locat (tilenum)
  "Location in texels of the tile number TILENUM."
  (nth tilenum (tiles-coords)))

(defun tex-tiles-coords-gl ()
  "Generate the list of texture tiles coords in GL shader language."
  (let ((s (apply #'cat
                  (mapcar #'(lambda (v)
                              (apply #'format
                                     nil
                                     "vec3( ~Ff , ~Ff , ~Ff ),"
                                     (vecxyz v)))
                          (tex-tiles-coords)))))
    (subseq s 0 (1- (length s)))))


(defun make-vertex-array ()
  "TODO Doc"
  ;; Because slices always parallel to the camera screen using 2D
  ;; vertexes is sufficient.
  (let ((slice-floats 8))
    (with-foreign-object (ar :float slice-floats)
      (setf (mem-aref ar :float 0) -.5f0
            (mem-aref ar :float 1) -.5f0
            (mem-aref ar :float 2) -.5f0
            (mem-aref ar :float 3)  .5f0
            (mem-aref ar :float 4)  .5f0
            (mem-aref ar :float 5) -.5f0
            (mem-aref ar :float 6)  .5f0
            (mem-aref ar :float 7)  .5f0)
      (gl:buffer-data :array-buffer
                      :static-draw
                      (gl::make-gl-array-from-pointer ar :float slice-floats))
      (gl:vertex-attrib-pointer +vertex-attrib-location+ 2 :float nil 0 0)
      (gl:enable-vertex-attrib-array +vertex-attrib-location+))))

(defun voxels-to-texture ()
  "Copy voxels of WOBS into the texture."
  ;; The rendering is not working yet.  Just load two tiles of the
  ;; first sprite
  (make-texture)
  (letvec (((x1 y1 z1) (tile-locat 0))
           ((x2 y2 z2) (tile-locat 1)))
    (gl:tex-sub-image-3d
     :texture-3d 0
     x1 y1 z1
     +voxmap-tile-size+ +voxmap-tile-size+ +voxmap-tile-size+
     :rgba :unsigned-byte
     (aref (voxmap-tiles (wob :voxmap (car *world*))) 0 0 0))
    (gl:tex-sub-image-3d
     :texture-3d 0
     x2 y2 z2
     +voxmap-tile-size+ +voxmap-tile-size+ +voxmap-tile-size+
     :rgba :unsigned-byte
     (aref (voxmap-tiles (wob :voxmap (car *world*))) 0 1 0))))

(let ((buffer 0)
      (vertex-array 0)
      (texture 0))
  (defun bind-gl-objects ()
    "Bind GL buffer, vertex array and texture."
    (when (and (zerop buffer)
               (zerop (setq buffer (car (gl:gen-buffers 1)))))
      (error "Can't generate a buffer"))
    (when (and (zerop vertex-array)
               (zerop (setq vertex-array (gl:gen-vertex-array))))
      (error "Can't generate a vertex array"))
    (when (and (zerop texture)
               (zerop (setq texture (car (gl:gen-textures 1)))))
      (error "Can't generate a texture"))
    (gl:bind-buffer :array-buffer buffer)
    (gl:bind-vertex-array vertex-array)
    (gl:bind-texture :texture-3d texture)
    (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-3d :texture-min-filter :linear)
    (make-vertex-array)
    (voxels-to-texture))

  (defun delete-gl-objects ()
    "Delete GL buffer, vertex array and texture."
    (unless (zerop buffer)
      (gl:delete-buffers (list buffer))
      (setq buffer 0))
    (unless (zerop vertex-array)
      (gl:delete-vertex-arrays (list vertex-array))
      (setq vertex-array 0))
    (unless (zerop texture)
      (gl:delete-textures (list texture))
      (setq texture 0))))

(defun render-frame ()
  (gl:clear :color-buffer)
  (when t                               ;(camera-sees)
    (load-uniforms)
    (%gl:draw-arrays-instanced :triangle-strip 0 4
                               (- *camera-far-plane* *camera-near-plane*)))
  (sdl2:gl-swap-window *render-window*))

(let ((program)
      (camera-far-index -1)
      (tex-screen-index -1))
  (defun load-uniforms ()
    "Load uniform variables."
    (gl:uniformf camera-far-index *camera-far-plane*)
    (gl:uniformf tex-screen-index (/ (camera-screen-size) (max-tex-size) 2)))

  (defun render-load-shaders (&optional
                                (vertex *default-vertex-shader*)
                                (fragment *default-fragment-shader*))
    "Initialize shader program."
    (setq program
          (let ((*package* (find-package :cleven)))
            (use-shaders (preprocess-shader
                          (read-file-into-string vertex))
                         (preprocess-shader
                          (read-file-into-string fragment))
                         (cons +vertex-attrib-location+ "position")))
          camera-far-index (gl:get-uniform-location program "camera_far_plane")
          tex-screen-index (gl:get-uniform-location program "tex_screen_size"))
    (let ((sampler (gl:get-uniform-location program "sampler")))
      (when (< sampler 0)
        (error "The sampler uniform is not found"))
      (gl:uniformi sampler 0)))

  (defun render-free-shaders ()
    "Free shader program."
    (free-shaders program)
    (setq program nil
          camera-far-index -1
          tex-screen-index -1)))

(defun define-reload-shaders ()
  "Define the render thread function `reload-shaders'."
  (defthreadfun reload-shaders (&optional
                                (vertex *default-vertex-shader*)
                                (fragment *default-fragment-shader*))
      nil
    (render-free-shaders)
    (render-load-shaders vertex fragment)))

;;; Put `render-frame' in `*render-loop-hook*' so we have ability to
;;; do something before or after the frame rendering.
(lock #'add-hook *render-loop-hook-lock* 'render-frame)
(lock #'add-hook *render-init-hook-lock* 'bind-gl-objects)
(lock #'add-hook *render-init-hook-lock* 'render-load-shaders)
(lock #'add-hook *render-init-hook-lock* 'define-reload-shaders)
(lock #'add-hook *render-init-hook-lock* 'init-texture-tiles)
(lock #'add-hook *render-quit-hook-lock* 'delete-gl-objects)
(lock #'add-hook *render-quit-hook-lock* 'render-free-shaders)
