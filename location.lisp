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

;;; TODO Use sb-cga

;;; Location is 3D vector

(defun locat (&optional (x 0) (y 0) (z 0))
  "Make a new location."
  (list x y z))

(defun locatn (&optional (n 0))
  "Make a location with elements equal to N."
  (locat n n n))

(declaim (inline locatx locaty locatz))

(defun locatx (loc)
  "TODO Doc"
  (car loc))

(defun locaty (loc)
  "TODO Doc"
  (cadr loc))

(defun locatz (loc)
  "TODO Doc"
  (caddr loc))

(defmacro deflocop (op)
  "Define a location operation OP."
  `(defun ,(symbolicate 'locat op) (loc-or-num &rest more)
     (format nil "Make a location applying ~A on elements of
LOC-OR-NUM and MORE.~%Numbers in (LOC-OR-NUM . MORE) are converted to
locations using `locatn'.  Elements of the result location are:

(~A loc11 loc21 loc31 ...),
(~A loc12 loc22 loc32 ...),
(~A loc13 loc23 loc33 ...),

where locNM is the location N in (LOC-OR-NUM . MORE) and its Mth element."
,op ,op ,op ,op)
     (apply #'mapcar
            #',op
            (mapcar #'(lambda (l)
                        (if (numberp l) (locatn l) l))
                    (cons loc-or-num more)))))

(macrolet ((defops (&rest ops)
             `(progn ,@(mapcar #'(lambda (op)
                                   `(deflocop ,op))
                               ops))))
  (defops + - * /))

(defun locat+! (loc1 loc2)
  "Sum elements of LOC1 and LOC2 and replace LOC1 with the result."
  (incf (car loc1) (car loc2))
  (incf (cadr loc1) (cadr loc2))
  (incf (caddr loc1) (caddr loc2))
  loc1)

(defun locat= (loc &rest more)
  "Return nil if locations of (LOC . MORE) are not equal."
  (apply #'every #'= loc more))

(defmacro with-locat ((loc &optional prefix) &body body)
  "Evaluate forms of BODY with elements of LOC bound to X Y Z.
If PREFIX non-nil, it should be symbol.  PREFIX added to the
elements names."
  (let ((vars (if prefix
                  (list (symbolicate prefix 'x)
                        (symbolicate prefix 'y)
                        (symbolicate prefix 'z))
                  (list 'x 'y 'z))))
    `(destructuring-bind (,(car vars) ,(cadr vars) ,(caddr vars)) ,loc
       ,@body)))

(defmacro with-locats ((&rest locs) &body body)
  "Evaluate forms of BODY with bound elements of each location.
Names of the elements are <name-of-location>{X,Y,Z}."
  (unless (every #'symbolp locs)
    (error "Elements of ~S must be symbols" locs))
  (labels ((db (ls)
             (let* ((l (car ls))
                    (x (symbolicate l 'x))
                    (y (symbolicate l 'y))
                    (z (symbolicate l 'z)))
               `(destructuring-bind (,x ,y ,z) ,l
                  ,@(if (endp (cdr ls))
                        body
                        (list (db (cdr ls))))))))
    (if locs
        (db locs)
        `(progn ,@body))))

;;; At the moment the location is just a list of coords.  But other
;;; code should not rely on this.
(declaim (inline locat-coords))
(defun locat-coords (loc)
  "Get list of elements (coordinates) of LOC."
  (copy-list loc))

(defun maplocat (fn loc)
  "Apply FN to each element of LOC."
  (apply #'locat
         (apply #'locat
                (mapcar fn (locat-coords loc)))))

(defun trunc-locat (loc &optional (divisor 1))
  "Truncate each element of LOC with optional DIVISOR."
  (maplocat #'(lambda (coord)
                (truncate coord divisor))
            loc))

(declaim (inline copy-locat))
(defun copy-locat (loc)
  "Make a copy of LOC."
  (copy-list loc))

(defun sort-box (l1 l2)
  "Rearrange the box [L1; L2] to [NEAREST; FARTHEST].
NEAREST and FARTHEST are the box corners that are nearest and farthest
to the origin respectively.

For example: L1 - the top-left corner, L2 - the bottom-right corner.

   ^  L1         F
   |   +--------+
   |   |        |
   |   |        |
   |   |        |
   |   +--------+
   |  N         L2
   |
   +--------------->
  O

Return a list two locations: the nearest corner, the farthest corner."
  (flet ((nf (l1 l2 i)
           (let ((n (elt l1 i))
                 (f (elt l2 i)))
             (if (< n f)
                 (list n f)
                 (list f n)))))
    (mapcar #'list (nf l1 l2 0) (nf l1 l2 1) (nf l1 l2 2))))

(defmacro dobox ((varloc loc1 loc2 &optional (step 1) (order '(x y z)))
                 &body body)
  "Iterate over the box [LOC1, LOC2] and evaluate forms of BODY.
VARLOC is the current location in the box.  STEP is the step by which
VARLOC is incremented.  ORDER is a row order of the iteration."
  (with-gensyms (x y z x1 y1 z1 x2 y2 z2 gstep)
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
      `(destructuring-bind ((,x1 ,y1 ,z1) (,x2 ,y2 ,z2)) (sort-box ,loc1 ,loc2)
         (let ((,gstep ,step))
           (loop ,@(mkfor (caddr order))
              do (loop ,@(mkfor (cadr order))
                    do (loop ,@(mkfor (car order))
                          do (let ((,varloc (locat ,x ,y ,z)))
                               ,@body)))))))))

(defun inboxp (loc loc1 loc2)
  "Return true if LOC inside the box [LOC1, LOC2]."
  (and (every #'>= loc loc1)
       (every #'<= loc loc2)))


(defmacro with-intersect (varl1 varl2 box1l1 box1l2 box2l1 box2l2 &body body)
  "If BOX1 and BOX2 intersect do BODY within the intersection."
  (with-gensyms (b1l1 b1l2 b2l1 b2l2)
    `(destructuring-bind (,b1l1 ,b1l2) (sort-box ,box1l1 ,box1l2)
       (destructuring-bind (,b2l1 ,b2l2) (sort-box ,box2l1 ,box2l2)
         (when (and (every #'<= ,b1l1 ,b2l2)
                    (every #'>= ,b1l2 ,b2l1))
           (let ((,varl1 (mapcar #'max ,b1l1 ,b2l1))
                 (,varl2 (mapcar #'min ,b1l2 ,b2l2)))
             ,@body))))))

(defun transform (matrix loc)
  "Apply transformation MATRIX to LOC.
Return a new location."
  (locat (+ (* (aref matrix 0 0) (car loc))
            (* (aref matrix 0 1) (cadr loc))
            (* (aref matrix 0 2) (caddr loc))
            (aref matrix 0 3))
         (+ (* (aref matrix 1 0) (car loc))
            (* (aref matrix 1 1) (cadr loc))
            (* (aref matrix 1 2) (caddr loc))
            (aref matrix 1 3))
         (+ (* (aref matrix 2 0) (car loc))
            (* (aref matrix 2 1) (cadr loc))
            (* (aref matrix 2 2) (caddr loc))
            (aref matrix 2 3))))

(defun loclen (loc)
  "Length of the vector LOC."
  (with-locat (loc)
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun norm (loc)
  "Normalize LOC.
Return a new location."
  (locat/ loc (loclen loc)))

(defun translation-matrix-loc (loc)
  "Make a translation matrix from LOC."
  (apply #'translation-matrix (locat-coords loc)))
