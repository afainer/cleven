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

(defconstant +voxel-size+ 4
  "Voxel size in bytes.
At the moment we have just one voxel format: RGBA.")

(defconstant +voxmap-tile-size+ 64
  "Tile size for all voxmaps.
For simplicity the tile size is constant.")

(defconstant +voxmap-tile-bytes+ (* +voxel-size+
                                    +voxmap-tile-size+
                                    +voxmap-tile-size+
                                    +voxmap-tile-size+)
  "Tile size in bytes.")

(defstruct voxmap
  (size (vec) :read-only t)
  (tiles nil :read-only t)
  (savefn nil :read-only t)
  (freefn nil :read-only t)
  (header nil))

(defcfun fopen :pointer
  (pathname :string)
  (mode :string))

(defcfun fclose :int
  (stream :pointer))

(defcfun fread :unsigned-int
  (buf :pointer)
  (size :unsigned-int)
  (nmemb :unsigned-int)
  (stream :pointer))

(defun load-voxels (file)
  "Allocate foreign memory and load voxels from FILE."
  (let* ((file (namestring file))
         (size (file-size file))
         (voxels (foreign-alloc :unsigned-char
                                :count size))
         (fstr (fopen file "r")))
    (unwind-protect-case ()
        (progn
          (if (null-pointer-p voxels)
              (error "Can't allocate memory for ~A" file))
          (if (null-pointer-p fstr)
              (error "Can't open ~A" file))
          (fread voxels 1 size fstr))
      (:abort (foreign-free voxels))
      (:always (fclose fstr)))
    voxels))

(defun save-voxels (voxels-file)
  "Save voxels for the voxel file"
  (declare (ignore voxels-file))
  ;; TODO Implement
  )

(declaim (inline voxmap-prop))
(defun voxmap-prop (voxmap prop)
  "Get the property PROP of VOXMAP.
Voxmap properties is a assoc list which is stored in a voxmap header."
  (cdr (assoc prop (voxmap-header voxmap) :test #'sym-eq)))

(defmacro with-voxmap-props ((voxmap-or-header &rest props) &body body)
  "Evaluate BODY with PROPS bound to values of header properties.
If VOXMAP-OR-HEADER is cons then it used as a header.  Otherwise it
should be a voxmap."
  (with-gensyms (gvoh gassoc)
    `(let* ((,gvoh ,voxmap-or-header)
            (,gassoc (if (consp ,gvoh)
                         #'(lambda (p) (cdr (assoc p ,gvoh :test #'sym-eq)))
                         #'(lambda (p) (voxmap-prop p ,gvoh))))
            ,@(mapcar #'(lambda (prop)
                          `(,prop (funcall ,gassoc ',prop)))
                      props))
       ,@body)))

(defun save-header (voxmap header voxfile)
  "Save HEADER of VOXMAP."
  (declare (ignore voxmap header voxfile))
  ;; TODO Implement
  )

(defun find-voxels (header-file voxels-file)
  "Get a pathname of VOXELS-FILE using the pathname HEADER-FILE.
If VOXELS-FILE has only name and type components then try to find it
where is HEADER-FILE.  Return VOXELS-FILE as is if it does not exist
here."
  (if (equal #P"" (pathname-directory-pathname voxels-file))
      (let ((v (dir+file (pathname-directory-pathname header-file)
                         voxels-file)))
        (if (file-exists-p v)
            v
            voxels-file))
      voxels-file))

(defun load-voxmap (file &optional mmap)
  "Load voxel map from FILE.
MMAP should have the following values:

  - nil: Do not use memory mapping to load the voxmap.

  - number: If the size of the voxmap is greater than the number, use
    memory mapping.  The voxmap size is a number of voxels
    (* width height depth).

  - other values: Use memory mapping."
  (let ((header (read-file file)))
    (with-voxmap-props (header voxels size)
      (setq voxels (find-voxels file voxels))
      (iflet* (or (and (numberp mmap) (> mmap (apply #'* (vecxyz size))))
                  mmap)
          ((mem (mmap-file voxels t) (load-voxels voxels))
           (addr (mmapped-addr mem) mem)
           (savefn #'(lambda (voxmap)
                       (save-header voxmap file voxels))
                   #'(lambda (voxmap)
                       (save-header voxmap file voxels)
                       (save-voxels voxels)))
           (freefn #'(lambda (voxmap save-p)
                       (when save-p (funcall savefn voxmap))
                       (munmap-file mem))
                   #'(lambda (voxmap save-p)
                       (when save-p (funcall savefn voxmap))
                       (foreign-free mem))))
        (if (null-pointer-p addr)
            (error "Could not allocate memory for the voxel map ~A" file)
            (unwind-protect-case ()
                (let* ((tsize (trunc-vec (apply #'vec size) +voxmap-tile-size+))
                       ;; TODO Convert TSIZE to a fixnum array
                       (tiles (make-array (mapcar #'truncate (nreverse (vecxyz tsize))))))
                  (dobox (v (vec) (vec- tsize (vecn 1)))
                    ;; TODO Convert V to a fixnum array
                    (setf (aref tiles
                                (truncate (vecz v))
                                (truncate (vecy v))
                                (truncate (vecx v)))
                          addr)
                    (incf-pointer addr +voxmap-tile-bytes+))
                  (make-voxmap :size size
                               :tiles tiles
                               :savefn savefn
                               :freefn freefn
                               :header header))
              (:abort (funcall freefn nil nil))))))))

(defun save-voxmap (voxmap)
  "Save voxels of VOXMAP."
  (funcall (voxmap-savefn voxmap) voxmap))

(defun free-voxmap (voxmap save-p)
  "Free VOXMAP."
  (funcall (voxmap-freefn voxmap) voxmap save-p))

(defmacro dovoxmap (voxmap &body body)
  "Iterate over VOXMAP tiles.
Bind variables TILE and TILELOC to the current tile and its location
in the voxmap; then evaluate forms of BODY."
  (with-gensyms (gloc gvoxmap)
    `(let ((,gvoxmap ,voxmap))
       (dobox (,gloc (vec)
                     (vec- (apply #'vec
                                  (nreverse (array-dimensions
                                             (voxmap-tiles ,gvoxmap))))
                           (vecn 1)))
         (let ((tile (apply #'aref
                            (voxmap-tiles ,gvoxmap)
                            ;; TODO Convert GLOC to a fixnum array
                            (mapcar #'truncate (nreverse (vecxyz ,gloc)))))
               (tileloc (vec* ,gloc +voxmap-tile-size+)))
           ,@body)))))
