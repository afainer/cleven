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
  (size (locat) :read-only t)
  (tiles nil :read-only t)
  (savefn nil :read-only t)
  (freefn nil :read-only t))

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
  ;; TODO Implement
  )

(defmacro destruct-voxmap-header (header-file &body body)
  "Evaluate forms of BODY within the voxmap header elements.
The header elements bound to the following variables:

VOXELS-FILE -- path to the voxels data.

SIZE -- width, height and depth of the voxmap in voxels; SIZE is
location."
  (with-gensyms (hdr hdrfile voxfile)
    `(flet ((a (item alist)
              (cdr (assoc item alist :test #'sym-eq))))
       (let* ((,hdrfile ,header-file)
              (,hdr (read-file ,hdrfile))
              (,voxfile (a 'voxels ,hdr))
              (voxels-file (if (pathname= (pathname-directory-pathname ,voxfile)
                                          #P"")
                               (dir+file (pathname-directory-pathname ,hdrfile)
                                         ,voxfile)
                               ,voxfile))
              (size (apply #'locat (a 'size ,hdr))))
         ,@body))))

(defun save-header (voxmap header voxfile)
  "Save HEADER of VOXMAP."
  ;; TODO Implement
  )

(defun load-voxmap (file &optional mmap)
  "Load voxel map from FILE.
MMAP should have the following values:

  - nil: Do not use memory mapping to load the voxmap.

  - number: If the size of the voxmap is greater than the number, use
    memory mapping.  The voxmap size is a number of voxels
    (* width height depth).

  - other values: Use memory mapping."
  (destruct-voxmap-header file
    (iflet* (or (and (numberp mmap) (> mmap (apply #'* (locat-coords size))))
                mmap)
        ((mem (mmap-file voxels-file t) (load-voxels voxels-file))
         (addr (mmapped-addr mem) mem)
         (savefn #'(lambda (voxmap)
                     (save-header voxmap file voxels-file))
                 #'(lambda (voxmap)
                     (save-header voxmap file voxels-file)
                     (save-voxels voxels-file)))
         (freefn #'(lambda (voxmap save-p)
                     (when save-p (funcall savefn voxmap))
                     (munmap-file mem))
                 #'(lambda (voxmap save-p)
                     (when save-p (funcall savefn voxmap))
                     (foreign-free mem))))
      (if (null-pointer-p addr)
          (error "Could not allocate memory for the voxel map ~A" file)
          (unwind-protect-case ()
              (let* ((tsize (trunc-locat size +voxmap-tile-size+))
                     (tiles (make-array (nreverse (locat-coords tsize)))))
                (dobox (loc (locat) (locat- tsize 1))
                  (with-locat (loc)
                    (setf (aref tiles z y x) addr))
                  (incf-pointer addr +voxmap-tile-bytes+))
                (make-voxmap :size size
                             :tiles tiles
                             :savefn savefn
                             :freefn freefn))
            (:abort (funcall freefn nil nil)))))))

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
       (dobox (,gloc (locat)
                     (locat- (apply #'locat
                                    (nreverse (array-dimensions
                                               (voxmap-tiles ,gvoxmap))))
                             1))
         (let ((tile (apply #'aref
                            (voxmap-tiles ,gvoxmap)
                            (nreverse (locat-coords ,gloc))))
               (tileloc (locat* ,gloc +voxmap-tile-size+)))
           ,@body)))))
