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

(defconstant +tile-floats+ (* 18 +voxmap-tile-size+)
  "Number of floats per render tile.
3 floats per vertex.  6 vertexes per slice.
+VOXMAP-TILE-SIZE+ slices per tile.")

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
          (init-texture-tiles)
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
            (dobox (loc
                    (locat)
                    (locatn (/ (1- max-tex-size) +voxmap-tile-size+)))
              (let ((l (locat* loc +voxmap-tile-size+)))
                (push l tiles-coords)
                (push (locat/ l max-tex-size) tex-tiles-coords)))
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
                  (mapcar #'(lambda (l)
                              (apply #'format
                                     nil
                                     "vec3( ~Ff , ~Ff , ~Ff ),"
                                     (locat-coords l)))
                          (tex-tiles-coords)))))
    (subseq s 0 (1- (length s)))))

(let ((buffer 0)
      (vertex-array 0)
      (texture 0))
  (defun ensure-bind ()
    "Ensure GL buffer, vertex array and texture are bound."
    (when (and (zerop buffer)
               (zerop (setq buffer (car (gl:gen-buffers 1)))))
      (error "Can't gererate a buffer"))
    (when (and (zerop vertex-array)
               (zerop (setq vertex-array (gl:gen-vertex-array))))
      (error "Can't gererate a vertex array"))
    (when (and (zerop texture)
               (zerop (setq texture (car (gl:gen-textures 1)))))
      (error "Can't gererate a texture"))
    (gl:bind-buffer :array-buffer buffer)
    (gl:bind-vertex-array vertex-array)
    (gl:bind-texture :texture-3d texture)
    (gl:tex-parameter :texture-3d :texture-mag-filter :linear)
    (gl:tex-parameter :texture-3d :texture-min-filter :linear))

  (defun free-tile ()
    "Delete the tile GL buffer, vertex array and texture."
    (unless (zerop buffer)
      (gl:delete-buffers (list buffer))
      (setq buffer 0))
    (unless (zerop vertex-array)
      (gl:delete-vertex-arrays (list vertex-array))
      (setq vertex-array 0))
    (unless (zerop texture)
      (gl:delete-textures (list texture))
      (setq texture 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun case-axis (axis xval yval zval)
    (case axis
      (x xval)
      (y yval)
      (z zval))))

(defun make-slice (&rest coords)
  "Make slice locations from the flat list of numbers."
  (mapcar #'(lambda (v)
              (apply #'locat v))
          (group coords 3)))

;;; TODO The set and fill should be re-entrant.  Generate closures as
;;; `make-fill-tslice'.
(let (slice dir ar verti)
  (defun set-vslice (vslice vdir glar vidx)
    (setq slice vslice
          dir vdir
          ar glar
          verti vidx))

  (defun fill-vslice ()
    "Fill the GL buffer with the current vertex slice."
    (dolist (v slice)
      (with-locat (v)
        (setf (mem-aref ar :float verti) (float x 0f0)
              (mem-aref ar :float (incf verti)) (float y 0f0)
              (mem-aref ar :float (incf verti)) (float z 0f0)))
      (incf verti)
      (locat+! v dir))))

(defun make-fill-tslice (slice dir ar texi wob rtileloc)
  "Make a function to fill the GL buffer with texture slices."
  (let* ((rot (wob :irotmat wob))
         (dir (transform rot (locat/ dir (max-tex-size))))
         (loc (locat/ (locat- (wob :location wob) rtileloc)
                      (max-tex-size)))
         (orig (locat/ (wob :origin wob) (max-tex-size))))
    (setq slice (mapcar #'(lambda (v)
                            (transform rot v))
                        slice))
    (values
     #'(lambda ()
         (dolist (v slice)
           (with-locat (v)
             (setf (mem-aref ar :float texi) (float x 0f0)
                   (mem-aref ar :float (incf texi)) (float y 0f0)
                   (mem-aref ar :float (incf texi)) (float z 0f0)))
           (incf texi)
           (locat+! v dir)))
     rot
     (matrix-locat
      (split-matrix (mat44mul (translation-matrix-loc orig)
                              rot
                              (translation-matrix-loc (locat- loc)))
                    t)))))

(let (woblocs wobnum texidx texnum rotmats emit)
  (defun make-fill-tslices (slice dir ar texis wobs rtileloc)
    "Make functions to fill the GL buffer with texture slices.
Also, make available the following: wobs locations, wobs number,
texture attribs index list, texture attribs number and wobs rotation
matrices.  See `wobs-locations', `wobs-number', `tex-attribs-indexes',
`tex-attribs-number', `wobs-rotmats'."
    (let (fns)
      (setq woblocs nil
            wobnum 0
            texidx nil
            texnum 0
            rotmats nil
            emit nil)
      (dolist (ob wobs)
        (multiple-value-bind (fn rot loc)
            (make-fill-tslice slice dir ar (car texis) ob rtileloc)
          (let ((i texnum))
            (if (some #'(lambda (m)
                          (decf i)
                          (wob :irotmat= ob m))
                      rotmats)
                (push i texidx)
                (progn
                  (push fn fns)
                  (push rot rotmats)
                  (push texnum texidx)
                  (incf texnum)
                  (pop texis)))
            (push loc woblocs)
            (push (if (wob :emit-light ob) 1 0) emit)
            (incf wobnum)
            ;; FIXME The rest of wobs may have ROT from ROTMATS.  We
            ;; should not to ignore them.
            (if (> texnum (max-tex-attribs))
                (return)))))
      (setq woblocs (nreverse woblocs)
            texidx (nreverse texidx)
            rotmats (nreverse rotmats)
            emit (nreverse emit))
      (nreverse fns)))

  (defun wobs-locations ()
    "Texture locations of wobs for the current render tile."
    woblocs)

  (defun wobs-number ()
    "Number of wobs for the current render tile."
    wobnum)

  (defun tex-attribs-indexes ()
    "The list of texture attribs indexes for the current render tile.
The texture attribs index is the index in texture attribs array for a
wob."
    texidx)

  (defun tex-attribs-number ()
    "Number of vertexes texture attribs for the current render tile."
    texnum)

  (defun wobs-rotmats ()
    "Wobs inverted rotations matrices."
    rotmats)

  (defun wobs-emit-light ()
    "Wobs which are simulate light emission."
    emit))

(macrolet
    ((mkslicesfn (axis sign)
       (let* ((xvslice '(make-slice
                         x y        z x (+ y ts) z        x y        (+ z ts)
                         x (+ y ts) z x y        (+ z ts) x (+ y ts) (+ z ts)))
              (yvslice '(make-slice
                         x        y z (+ x ts) y z        x        y (+ z ts)
                         (+ x ts) y z x        y (+ z ts) (+ x ts) y (+ z ts)))
              (zvslice '(make-slice
                         x        y z (+ x ts) y        z x (+ y ts) z
                         (+ x ts) y z (+ x ts) (+ y ts) z x (+ y ts) z))
              (xtslice '(make-slice
                         at t1y t1z at t2y t1z at t1y t2z
                         at t2y t1z at t1y t2z at t2y t2z))
              (ytslice '(make-slice
                         t1x at t1z t2x at t1z t1x at t2z
                         t2x at t1z t1x at t2z t2x at t2z))
              (ztslice '(make-slice
                         t1x t1y at t2x t1y at t1x t2y at
                         t2x t1y at t2x t2y at t1x t2y at))
              (vslice (case-axis axis xvslice yvslice zvslice))
              (tslice (case-axis axis xtslice ytslice ztslice))
              (dir    (case-axis axis
                                 `(locat (,sign 1) 0 0)
                                 `(locat 0 (,sign 1) 0)
                                 `(locat 0 0 (,sign 1)))))
         `#'(lambda (loc glar verti texis wobs)
              (with-locat (loc)
                ;; FIXME Half-texel correction should be peformed in
                ;; `init-texture-tiles'.
                ;; TODO Use `with-locats'
                (with-locat ((locatn (/ .5f0 (max-tex-size))) t1)
                  (declare (ignorable t1x t1y t1z))
                  (with-locat ((locatn (- (tex-tile-size)
                                          (/ .5f0 (max-tex-size))))
                               t2)
                    (declare (ignorable t2x t2y t2z))
                    (let* ((ts +voxmap-tile-size+)
                           (,axis ,(if (eq sign '-) `(+ ts ,axis) axis))
                           (at ,(if (eq sign '-)
                                    (symbolicate 't2 axis)
                                    (symbolicate 't1 axis)))
                           (fill-tslices (make-fill-tslices ,tslice
                                                            ,dir
                                                            glar
                                                            texis
                                                            wobs
                                                            loc)))
                      (set-vslice ,vslice ,dir glar verti)
                      (dotimes (s +voxmap-tile-size+)
                        (fill-vslice)
                        (mapc #'funcall fill-tslices))))))))))
  (let ((slfn+x (mkslicesfn x +)) (slfn-x (mkslicesfn x -))
        (slfn+y (mkslicesfn y +)) (slfn-y (mkslicesfn y -))
        (slfn+z (mkslicesfn z +)) (slfn-z (mkslicesfn z -)))
    (defun make-slices (loc glar verti texis wobs)
      "Make vertex and texture slices and fill the bound GL buffer."
      (with-locat ((camera-vector))
        (let ((coords (list
                       (cons (abs x) (if (> x 0) slfn-x slfn+x))
                       (cons (abs y) (if (> y 0) slfn-y slfn+y))
                       (cons (abs z) (if (> z 0) slfn-z slfn+z)))))
          (funcall (cdr (reduce #'(lambda (x y)
                                    (if (> (car x) (car y)) x y))
                                coords))
                   loc glar verti texis wobs))))))

(defun make-render-tile (loc wobs)
  "Make proxy geometry for the rendering tile at location LOC.
WOBS is a list of wobs in the tile."
  (let ((vi 0)
        (ti +tile-floats+))
    (with-foreign-object (ar :float (+ +tile-floats+
                                       (* (max-tex-attribs)
                                          +tile-floats+)))
      (progn
        (make-slices loc ar vi
                     (map0-n #'(lambda (i)
                                 (+ i ti))
                             (* (1- (max-tex-attribs)) +tile-floats+)
                             +tile-floats+)
                     wobs)
        (ensure-bind)
        (gl:buffer-data :array-buffer
                        :static-draw
                        (gl::make-gl-array-from-pointer
                         ar
                         :float
                         (+ +tile-floats+
                            (* (tex-attribs-number)
                               +tile-floats+))))
        ;; 3 floats per vertex, 3 float per texture coordinate
        (gl:vertex-attrib-pointer +vertex-attrib-location+
                                  3 :float nil 0 0)
        (gl:enable-vertex-attrib-array +vertex-attrib-location+)
        (loop for i from 1 to (tex-attribs-number)
           with attr
           do
             (setq attr (+ i +vertex-attrib-location+))
             (gl:vertex-attrib-pointer
              attr 3 :float nil 0
              (* i ti (cffi:foreign-type-size :float)))
             (gl:enable-vertex-attrib-array attr))))))

(defun voxels-to-texture (wobs)
  "Copy voxels of WOBS into the texture."
  (make-texture)
  (let ((i -1))
    (dolist (o wobs)
      (with-locat ((tile-locat (incf i)))
        (gl:tex-sub-image-3d
         :texture-3d 0
         x y z
         +voxmap-tile-size+ +voxmap-tile-size+ +voxmap-tile-size+
         :rgba :unsigned-byte
         (wob :voxels o))))))

(defun render-frame ()
  (gl:clear :color-buffer)
  (when *world*
    (do-camera-box loc
      (let ((wobs (remove-if-not #'(lambda (o)
                                     (wob :voxels o))
                                 (world-tile loc))))
        (when wobs
          (make-render-tile loc wobs)
          (voxels-to-texture wobs)
          (load-uniforms)
          (gl:draw-arrays :triangles 0 (* 6 +voxmap-tile-size+))))))
  (sdl2:gl-swap-window *render-window*))

(let ((program)
      (camera-index -1)
      (texnum-index -1)
      (diffvecx-index -1)
      (diffvecy-index -1)
      (diffvecz-index -1)
      (woblocs-index -1)
      (wobnum-index -1)
      (texidx-index -1)
      (emitlight-index -1))
  (defun load-uniforms ()
    "Load uniform variables."
    (flet ((load-diffvec ()
             "Set difference vectors for WOBS.
Central difference gradient estimation requires values of neighbour
voxels.  The addressing such neighbours from the texture should take
into account different wobs orientations."
             ;; 3 floats per vector
             (let ((n (* (tex-attribs-number) 3))
                   (i 0)
                   (d (/ 1 (max-tex-size))))
               (with-foreign-objects ((ax '%gl:float n)
                                      (ay '%gl:float n)
                                      (az '%gl:float n))
                 (dolist (r (wobs-rotmats))
                   (let ((x (transform r (locat d)))
                         (y (transform r (locat 0 d)))
                         (z (transform r (locat 0 0 d))))
                     (with-locats (x y z)
                       (setf (mem-aref ax '%gl:float (+ i 0)) xx
                             (mem-aref ax '%gl:float (+ i 1)) xy
                             (mem-aref ax '%gl:float (+ i 2)) xz
                             (mem-aref ay '%gl:float (+ i 0)) yx
                             (mem-aref ay '%gl:float (+ i 1)) yy
                             (mem-aref ay '%gl:float (+ i 2)) yz
                             (mem-aref az '%gl:float (+ i 0)) zx
                             (mem-aref az '%gl:float (+ i 1)) zy
                             (mem-aref az '%gl:float (+ i 2)) zz)
                       (incf i 3))))
                 (%gl:uniform-3fv diffvecx-index n ax)
                 (%gl:uniform-3fv diffvecy-index n ay)
                 (%gl:uniform-3fv diffvecz-index n az))))

           (load-woblocs ()
             (with-foreign-object (ar '%gl:float (* 3 (wobs-number)))
               (let ((i 0))
                 (dolist (l (wobs-locations))
                   (with-locat (l)
                     (setf (mem-aref ar '%gl:float (+ i 0)) x
                           (mem-aref ar '%gl:float (+ i 1)) y
                           (mem-aref ar '%gl:float (+ i 2)) z)
                     (incf i 3))))
               (%gl:uniform-3fv woblocs-index (wobs-number) ar)))

           (load-texidx ()
             (with-foreign-object (ar '%gl:int (wobs-number))
               (let ((i -1))
                 (dolist (n (tex-attribs-indexes))
                   (setf (mem-aref ar '%gl:int (incf i)) n)))
               (%gl:uniform-1iv texidx-index (wobs-number) ar)))

           (load-emitidx ()
             (with-foreign-object (ar '%gl:int (wobs-number))
               (let ((i -1))
                 (dolist (n (wobs-emit-light))
                   (setf (mem-aref ar '%gl:int (incf i)) n)))
               (%gl:uniform-1iv emitlight-index (wobs-number) ar))))

      (gl:uniform-matrix camera-index 4 (vector (camera-view)))
      (gl:uniformi texnum-index (tex-attribs-number))
      (load-diffvec)
      (load-woblocs)
      (gl:uniformi wobnum-index (wobs-number))
      (load-texidx)
      (load-emitidx)))

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
          camera-index (gl:get-uniform-location program "camera")
          texnum-index (gl:get-uniform-location program "texnum")
          diffvecx-index (gl:get-uniform-location program "diffvecx")
          diffvecy-index (gl:get-uniform-location program "diffvecy")
          diffvecz-index (gl:get-uniform-location program "diffvecz")
          woblocs-index (gl:get-uniform-location program "woblocs")
          wobnum-index (gl:get-uniform-location program "wobnum")
          texidx-index (gl:get-uniform-location program "texidx")
          emitlight-index (gl:get-uniform-location program "emitlightidx"))
    (let ((sampler (gl:get-uniform-location program "sampler")))
      (when (< sampler 0)
        (error "The sampler uniform is not found"))
      (gl:uniformi sampler 0)))

  (defun render-free-shaders ()
    "Free shader program."
    (free-shaders program)
    (setq program nil
          camera-index -1
          texnum-index -1
          diffvecx-index -1
          diffvecy-index -1
          diffvecz-index -1
          woblocs-index -1
          wobnum-index -1
          texidx-index -1
          emitlight-index -1)))

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
(lock #'add-hook *render-quit-hook-lock* 'free-tile)
(lock #'add-hook *render-init-hook-lock* 'render-load-shaders)
(lock #'add-hook *render-init-hook-lock* 'define-reload-shaders)
(lock #'add-hook *render-quit-hook-lock* 'render-free-shaders)
