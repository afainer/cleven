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

(declaim (inline vec))
(defun vec (&optional (x 0f0) (y 0f0) (z 0f0))
  "Allocate a 3d vector [X, Y, Z]."
  (sb-cga:vec (float x 0f0) (float y 0f0) (float z 0f0)))

(define-compiler-macro vec (&whole form &optional (x 0f0) (y 0f0) (z 0f0))
  (if (endp (cdr form))
      '(sb-cga:alloc-vec)
      (if (not (or (numberp x) (numberp y) (numberp z)))
          form
          `(sb-cga:vec ,(if (numberp x) (float x 0f0) `(float ,x 0f0))
                       ,(if (numberp y) (float y 0f0) `(float ,y 0f0))
                       ,(if (numberp z) (float z 0f0) `(float ,z 0f0))))))

(declaim (inline vecn))
(defun vecn (n)
  "Allocate a 3d vector [N, N, N]."
  (let ((n (float n 0f0)))
    (sb-cga:vec n n n)))

(declaim (inline vecx vecy vecz vecxyz vec* vec/ vec+!))

(defun vecx (v)
  "The element X of the vector V."
  (aref v 0))

(defun vecy (v)
  "The element Y of the vector V."
  (aref v 1))

(defun vecz (v)
  "The element Z of the vector V."
  (aref v 2))

(defun vecxyz (v)
  "The elements list of V."
  (coerce v 'list))

(defun vec* (v n)
  "Multiply vec V with number N."
  (sb-cga:vec* v (float n 0f0)))

(defun vec/ (v n)
  "Divide vec V by number N."
  (sb-cga:vec/ v (float n 0f0)))

(defun vec+! (a b)
  "Add A and B, store result in A."
  (sb-cga:%vec+ a a b))

(defun trunc-vec (v &optional (divisor 1))
  "Truncate each element of V with optional DIVISOR."
  (vec (truncate (vecx v) divisor)
       (truncate (vecy v) divisor)
       (truncate (vecz v) divisor)))

(defmacro letvec ((&rest bindings) &body body)
  "Bind elements of BINDINGS and evaluate the forms of BODY.
BINDINGS is the following list:

\({pref | (pref [value]) | ((x y z) value)}*)

Bind prefX, prefY, prefZ to the elements of the result vector VALUE.
In case of form ((x y z) value) bind X Y Z to the elements of VALUE.

Examples:
  (letvec ((a (vec 1 2 3))
           ((x y z) (vec 4 5 6))
           b
           (c))
     (list ax by cz x y z)"
  (flet ((elts-names (vec-name)
           (list (symbolicate vec-name 'x)
                 (symbolicate vec-name 'y)
                 (symbolicate vec-name 'z))))
    (let* ((bindings (mapcar #'(lambda (b)
                                 (if (consp b)
                                     (if (consp (car b))
                                         b
                                         (cons (elts-names (car b)) (cdr b)))
                                     (list (elts-names b))))
                             bindings))
           (gvecs (mapcar #'(lambda (b)
                              `(,(gensym) ,(if (cdr b)
                                               (cadr b)
                                               '(sb-cga:alloc-vec))))
                          bindings)))
      `(let ,gvecs
         (let ,(mapcan #'(lambda (b g)
                           (list `(,(caar b) (vecx ,(car g)))
                                 `(,(cadar b) (vecy ,(car g)))
                                 `(,(caddar b) (vecz ,(car g)))))
                       bindings
                       gvecs)
           (declare (ignorable ,@(mappend #'car bindings)))
           ,@body)))))

(defmacro with-vecs ((&rest vecs) &body body)
  "Bind elements of VECS and evaluate the forms of BODY.
VECS is a list of symbols: (VEC1 VEC2...).  Each symbol should be
bound to a vector.  Bind VEC1X, VEC1Y, VEC1Z, VEC2X, VEC2Y,
VEC2Z... to elements of VEC1, VEC2 and so on. "
  `(letvec ,(mapcar #'(lambda (v) `(,v ,v)) vecs)
     ,@body))

(defmacro dobox ((varloc vec1 vec2 &optional (step 1) (order '(x y z)))
                 &body body)
  "Iterate over the box [VEC1, VEC2] and evaluate forms of BODY.
VARLOC is the current location in the box.  STEP is the step by which
VARLOC is incremented.  ORDER is a row order of the iteration."
  (with-gensyms (x y z x1 y1 z1 x2 y2 z2 gvec1 gvec2 gstep)
    (flet ((mkfor (var)
             (let* ((name (symbol-name var))
                    (to (if (char/= (char name 0) #\-)
                            'to
                            (progn
                              (setq name (subseq name 1))
                              'downto)))
                    (vars (ecase (intern name)
                            (x (list x x1 x2))
                            (y (list y y1 y2))
                            (z (list z z1 z2)))))
               (when (eq to 'downto)
                 (setq vars (list (car vars) (caddr vars) (cadr vars))))
               `(for ,(car vars) from ,(cadr vars) ,to ,(caddr vars) by ,gstep))))
      `(let ((,gvec1 ,vec1)
             (,gvec2 ,vec2)
             (,gstep ,step))
         (letvec (((,x1 ,y1 ,z1) (vec-min ,gvec1 ,gvec2))
                  ((,x2 ,y2 ,z2) (vec-max ,gvec1 ,gvec2)))
           (loop ,@(mkfor (caddr order))
              do (loop ,@(mkfor (cadr order))
                    do (loop ,@(mkfor (car order))
                          do (let ((,varloc (vec ,x ,y ,z)))
                               ,@body)))))))))

(defun inboxp (vec vec1 vec2)
  "Is the point VEC is inside the box [VEC1, VEC2]."
  (and (every #'>= vec vec1)
       (every #'<= vec vec2)))

(declaim (inline rotate* rotated* rotated))
(defun rotate* (x y z)
  "Construct a rotation matrix from rotation factors X, Y, Z."
  (sb-cga:rotate* (float x 0f0) (float y 0f0) (float z 0f0)))

(defun rotated* (x y z)
  "Construct a rotation matrix from rotation factors X, Y, Z.
The factors given in degrees."
  (sb-cga:rotate* (deg2rad x) (deg2rad y) (deg2rad z)))

(defun rotated (vec)
  "Construct a rotation matrix using first three elements of VEC as
the rotation factors.  The elements given in degrees."
  (rotated* (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (inline translatev))
(defun translatev (mat)
  "Construct a vector from the translation elements of MAT."
  (sb-cga:vec (mref mat 0 3) (mref mat 1 3) (mref mat 2 3)))

(declaim (inline row-major-mref))
(defun row-major-mref (matrix index)
  "The row-major order element INDEX of MATRIX.
Consider MATRIX as a vector by viewing its elements in row-major
order.  The row-major order element INDEX is the element referred to
by INDEX."
  (aref matrix index))

(defun matrix3 (m11 m12 m13
                m21 m22 m23
                m31 m32 m33)
  "Make a matrix specifying its 3x3 submatrix.
Other elements but the last have zero value.  The last 4x4 element
value is 1."
  (matrix m11 m12 m13 0f0
          m21 m22 m23 0f0
          m31 m32 m33 0f0
          0f0 0f0 0f0 1f0))
