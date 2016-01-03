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


;;; Lists

(declaim (inline carl))
(defun carl (l)
  "Return a list with car of the list L."
  (list (car l)))

(declaim (inline cons-if))
(defun cons-if (x y)
  "Return (cons X Y) if X is non-nil, otherwise return Y."
  (if x (cons x y) y))

(declaim (inline cons-if*))
(defun cons-if* (x y z)
  "Return (cons Y Z) if X is non-nil, otherwise return Z."
  (if x (cons y z) z))

(declaim (inline last*))
(defun last* (l)
  "Return car of the last cons of the list L."
  (car (last l)))

(declaim (inline group))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (l n)
    "Group the list L to sublists of N elements."
    (when (plusp n)
      (loop with l1 = l
         until (endp l1)
         collect (loop
                    for i from 1 to n
                    until (endp l1)
                    collect (pop l1))))))

;;; Control flow

(defmacro aif (test then &optional else)
  "Assign TEST to the variable IT. If IT is non-nil evaluate THEN.
Otherwise evaluate ELSE. The name `aif' stands for `anaphoric if'."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro aif* (expr test then &optional else)
  "Bind value of EXPR to IT and if TEST is non-nil evaluate THEN.
Otherwise evaluate ELSE."
  `(let ((it ,expr))
     (if ,test ,then ,else)))

(defmacro awhen (test &body forms)
  "Assign TEST to the variable IT. If IT is non-nil evaluate FORMS.
Otherwise return NIL. The name `awhen' stands for `anaphoric when'."
  `(let ((it ,test))
     (when it ,@forms)))

;;; Isn't it that using just '(or test (progn forms...))' have same results?
(defmacro unless* (test &body forms)
  "Return the evaluation result of the form TEST if it is non-nil.
Otherwise evaluate FORMS and return the result of the last evaluated
form."
  (let ((val (gensym)))
    `(let ((,val ,test))
       (or ,val (progn ,@forms)))))

;;; Bindings

;;; TODO Check value of TEST in expansion-time. If it is nil or t we
;;; can make the right expansion (equvivalent to `if').
(macrolet
    ((defiflet (let-op)
       `(defmacro ,(symbolicate 'if let-op) (test bindings &body body)
          ,(format nil
"Evaluate BODY with variables bound to then or else values.
Elements of the list BINDINGS are the following list:

  (var then-value [else-value]).

If the test form TEST is non-nil a new variable bound to then-value
otherwise to else-value. If else-value is not supported it has value
`nil'.

The form

  (if~A test
       ((var1 then1 else1)
        (var2 then2))
     body)

is equivalent to the form

  (if test
      (~A ((var1 then1)
           (var2 then2))
        body)
      (~A ((var1 else1)
           (var2 nil))
        body))." let-op let-op let-op)
          (let (then else)
            (dolist (b bindings)
              (destructuring-bind (v th &optional el) b
                (push (list v th) then)
                (push (list v el) else)))
            `(if ,test
                 (,',let-op ,(nreverse then) ,@body)
                 (,',let-op ,(nreverse else) ,@body))))))
  (defiflet let)
  (defiflet let*))

(defmacro ifdbind (test then-lambdalist then-form
                   else-lambdalist else-form
                   &body body)
  "Evaluate the forms BODY with destructured THEN-FORM or ELSE-FORM.

If the form TEST is non-nil new variables specified in
THEN-LAMBDALIST are bound to the corresponding values in THEN-FORM.
Otherwise variables specified in ELSE-LAMBDALIST are bound to the
corresponding values in ELSE-FORM.

The form

  (ifdbind test
           then-lambdalist then-form
           else-lambdalist else-form
           body)

is equivalent to the form

  (if test
      (destructuring-bind then-lambdalist then-form body)
      (destructuring-bind else-lambdalist else-form body))."
  `(if ,test
       (destructuring-bind ,then-lambdalist ,then-form ,@body)
       (destructuring-bind ,else-lambdalist ,else-form ,@body)))

;;; Loops

(defmacro while (test &body body)
  "Evaluate the forms of BODY while the expression TEST is non-nil."
  `(loop
      while ,test
      do (progn ,@body)))

;;; It would be nice to have several variables:
;;; (for ((x 1 y 2) test next) ...)
(defmacro for
    ((var begin test next &key prebegin postbegin preend postend)
     &body body)
  "Bind VAR to BEGIN and evaluate BODY while TEST is non-nil.

At the end of each iteration change the value of VAR to the value of
the expression NEXT. The bound variable VAR is visible to the forms of
BODY and the expressions TEST and NEXT.

Use expressions PREBEGIN, POSTBEGIN, PREEND, POSTEND as follows.

- Evaluate PREBEGIN just before the first iteration.
- Evaluate POSTBEGIN just after the first iteration.
- Evaluate PREEND just before the last iteration.
- Evaluate POSTEND just after the last iteration.
- If no iterations are performed evaluate PREBEGIN then POSTEND.

If you rely on side effects in BODY made by TEST note that TEST is
evaluated two times before the first evaluation of BODY, so the TEST
evaluation is ahead of the BODY evaluation by one iteration.  Thus we
determine when to evaluate PREEND."
  (with-gensyms (gbody gpreend)
    (flet ((lamb (v b &optional e)
             `(let ((,var ,v))
                #'(lambda () ,@b ,e ,var))))
      `(let* ((,var ,begin)
              (,gpreend ,(lamb var (list preend)))
              (,gbody ,(lamb var body postbegin)))
         ,prebegin
         (when ,test
           (setq ,var ,next)
           (while ,test
             (setq ,gpreend ,(lamb `(funcall ,gbody) (list preend))
                   ,gbody ,(lamb var body))
             (setq ,var ,next))
           (funcall ,gpreend)
           (setq ,var (funcall ,gbody)))
         ,postend))))

;;; Strings

(declaim (inline cat))
(defun cat (&rest sequences)
  "Convert SEQUENCES to strings and `concatenate' them."
  (apply #'concatenate 'string
         (mapcar #'string sequences)))

;;; Symbols

(defun sym-eq (s1 s2 &rest more)
  "Compare symbols S1 S2 and MORE regardless of their packages."
  (let ((n (symbol-name s1)))
    (every #'(lambda (s)
               (string= n (symbol-name s)))
           (cons s2 more))))

;;; Pathnames

(declaim (inline dir+file))
(defun dir+file (dir file)
  "Return the pathname DIR appended with the pathname FILE."
  (merge-pathnames file
                   (pathname-as-directory dir)))

;;; TODO Make dirnames analogue.
;;; TODO Remove this in favor of `cl-fad:pathname-parent-directory'.
(defun dirup (dir)
  "Return the parent directory of the pathname DIR.
Return the pathname DIR without parent directories as the second
value."
  (let ((struct (pathname-directory dir)))
    (values (make-pathname :directory (butlast struct))
            (last* struct))))

;;; FIXME (pathname-eq "~/tmp/" "/home/user/tmp/") => NIL
(defun pathname= (p1 p2 &rest more)
  "Return T if P1 and P2 are equal pathnames.
Both arguments are converted to strings by `namestring' and compared
with `string=' so P1 and P2 can be strings or pathnames. Use the
argument MORE to compare more than two pathnames."
  (let ((s (namestring p1)))
   (every #'(lambda (p)
              (string= s p))
          (mapcar #'namestring (cons p2 more)))))

;; FIXME The function can't move directories to another devices at
;; least for SBCL.
(defun rename-directory (old new)
  "Rename the directory OLD to NEW.
Return NEW directory."
  #+clisp (ext:rename-directory old new)
  #+sbcl (eq new (rename-file old new))
  #-(or sbcl clisp)
  (error "RENAME-DIRECTORY is implemented for SBCL and CLISP only.")
  new)

;;; Numbers

;;; TODO Add clamp-min and clamp-max

(declaim (inline ceil))
(defun ceil (number &optional (divisor 1))
  "if DIVISOR is 1 return (ceiling NUMBER).
Otherwise, return (* NUMBER (ceiling NUMBER/DIVISOR))."
  (if (= divisor 1)
      (ceiling number)
      (- number
         (cadr (multiple-value-list (ceiling number divisor))))))

(declaim (inline mapa-b))
(defun mapa-b (fn a b &optional (step 1))
  "Apply the function FN to the range [A; B] with the optional STEP."
  (loop for i from a to b by step
       collect (funcall fn i)))

(declaim (inline map0-n))
(defun map0-n (fn n &optional (step 1))
  "Apply the function FN to the range [0; N] with the optional STEP."
  (mapa-b fn 0 n step))

(declaim (inline map1-n))
(defun map1-n (fn n &optional (step 1))
  "Apply the function FN to the range [1; N] with the optional STEP."
  (mapa-b fn 1 n step))

;;; Types

(defmacro fcoerce (obj &optional llist &body clauses)
  "Convert the object OBJ to a function if it is not.
LLIST is a lambda-list for the created function.  CLAUSES is the
following list:
  (TEST1 FORM1 TEST2 FORM2 ... TESTN FORMN [FORMD]),

If the expression TEST1 is non-nil then create a function with FORM1
as its body, otherwise if TEST2 is non-nil create a function with
FORM2 and so on.  If all tests including the last TESTN are `nil'
create a function with optional FORMD.

Before conversion bind the variable IT to the result of OBJ
evaluation, so IT is visible to all tests and to the created
function."
  `(let ((it ,obj))
    (cond
      ((functionp it) it)
      ,@(mapcar #'(lambda (c)
                    (if (cdr c)
                        `(,(car c)
                           #'(lambda ,llist ,(cadr c)))
                        `(t #'(lambda ,llist ,(car c)))))
                (group clauses 2))
      ;; FIXME Ignore all vars in llist
      (t #'(lambda ,llist it)))))

;;; IO

(defun map-file (fn filename &optional (if-does-not-exist :error)
                 (end nil end-p))
  "Apply the function FN to each expression in the file FILENAME.
Return a list of applying results.

The argument IF-DOES-NOT-EXIST have the same meaning as the
corresponding argument of the function `open'. Its default value
is :ERROR.

If the argument END is a function apply it on each result of applying
the function FN. Stop reading the file FILENAME if the function END
returns T. If the argument END any other object compare it with each
result using the function `equal'. If the argument END is not used
read the file FILENAME until the end of the file is reached."
  (with-open-file (s filename :direction :input
                     :if-does-not-exist if-does-not-exist)
    (when s
      (let ((eof (gensym))
            (end (fcoerce end (x)
                   end-p (equal end x)
                   (declare (ignore x)))))
        (loop
           for r = (read s nil eof)
           with f
           until (eq r eof)
           do (setq f (funcall fn r))
           until (funcall end f)
           collect f)))))

(defun read-file (filename &optional (if-does-not-exist :error))
  "Read expressions in the file FILENAME until EOF is reached.
Return a list of the expessions. The value of the argument
IF-DOES-NOT-EXIST have the same meaning as the value of the
corresponding argument of the function `open'. Its default value is
:ERROR."
  (map-file #'identity filename if-does-not-exist))

;; FIXME It is inconvenient to write one object.  You have to wrap it
;; into a list (write-file (list obj) file)
(defun write-file (filename list &optional (if-exists :error)
                   (delimiter #'terpri))
  "Write objects in the list LIST to the file FILENAME.

The value of the argument IF-EXISTS have the same meaning as the value
of the corresponding argument of the function `open'. Its default
value is :ERROR.

DELIMITER is a function that takes a stream. It writes a delimiter to
the file FILENAME after each written object from the list LIST."
  (with-open-file (f filename :direction :output
                     :if-does-not-exist :create
                     :if-exists if-exists)
    (dolist (i list list)
      (write i :stream f)
      (funcall delimiter f))))

(defun file-size (pathname)
  "Return size of the file PATHNAME in bytes."
  (with-open-file (f pathname :direction :input
                     :element-type 'unsigned-byte)
    (file-length f)))

;;; Hooks manipulation functions.

(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.  FUNCTION is added at the
beginning of the hook list unless the optional argument APPEND is
non-nil, in which case FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function or
symbol bound to a function."
  (symbol-macrolet ((svh (symbol-value hook)))
    (unless (member function svh)
      (if append
          (setf svh (nconc svh (list function)))
          (push function svh)))))

(defun remove-hook (hook function)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function or
symbol.  If FUNCTION doesn't appear in the list of hooks to run in
HOOK, then nothing is done.  See `add-hook'."
  (setf (symbol-value hook)
        (delete function
                (symbol-value hook))))

;;; FIXME If some function signals an error we should have a choice to
;;; run the rest of functions or abort.
(defun run-hooks (&rest hooks)
  "Run each hook in HOOKS.
Each argument should be a symbol, a hook variable.  HOOKS are
processed in the order specified.  The elements of each hook are
called, in order, with no arguments."
  (dolist (hook hooks)
    (dolist (h (symbol-value hook))
      (funcall h))))

(defun run-hook-with-args (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Call each function HOOK in
order with arguments ARGS."
  (dolist (h (symbol-value hook))
    (apply h args)))

;;; Multithreading

(defun make-shared (obj &optional name)
  "Make a shared object from OBJ."
  (cons (bt:make-lock name) obj))

(defun lock (fn shared &rest args)
  "Lock SHARED and apply FN on it with ARGS."
  (bt:with-lock-held ((car shared))
    (apply fn (cdr shared) args)))

(defun lock* (fn &rest args)
  "Lock the shared object and apply FN on it with ARGS.
The shared object is the last element in ARGS."
  (let* ((o (last* args))
         (a (butlast args)))
    (setq a (nconc a (list (cdr o))))
    (bt:with-lock-held ((car o))
      (apply fn a))))

(defmacro with-lock ((var shared) &body body)
  "Execute the forms BODY within the locked SHARED.
The variable VAR is bound to the value of SHARED."
  (let ((gshared (gensym)))
    `(let* ((,gshared ,shared)
            (,var (cdr ,gshared)))
       (bt:with-lock-held ((car ,gshared))
         ,@body))))

;;; Math

(declaim (inline deg2rad))
(defun deg2rad (x)
  "Convert degrees to radians."
  (* x (/ (float pi) 180)))

(declaim (inline cods))
(defun cosd (x)
  "Cosine of X.
X in degrees."
  (cos (deg2rad x)))

(declaim (inline sind))
(defun sind (x)
  "Sine of X.
X in degrees."
  (sin (deg2rad x)))

;;; TODO Use sb-cga

(declaim (inline identity-matrix))
(defun identity-matrix ()
  "Make an identity matrix."
  (make-array '(4 4)
              :initial-contents
              '((1 0 0 0)
                (0 1 0 0)
                (0 0 1 0)
                (0 0 0 1))))

(defun mat44mul (&rest matrices)
  "Trivial matrix multiplication.
Multiplications are applied in reverse order, i.e. the last matrix is
applied first."
  (flet ((mul (m1 m2)
           (flet ((m (i j)
                    (+ (* (aref m1 i 0) (aref m2 0 j))
                       (* (aref m1 i 1) (aref m2 1 j))
                       (* (aref m1 i 2) (aref m2 2 j))
                       (* (aref m1 i 3) (aref m2 3 j)))))
             (make-array '(4 4)
                         :initial-contents
                         (list
                          (list (m 0 0) (m 0 1) (m 0 2) (m 0 3))
                          (list (m 1 0) (m 1 1) (m 1 2) (m 1 3))
                          (list (m 2 0) (m 2 1) (m 2 2) (m 2 3))
                          (list (m 3 0) (m 3 1) (m 3 2) (m 3 3)))))))
    (if (null matrices)
        (identity-matrix)
        (reduce #'mul matrices))))

(defun translation-matrix (x y z)
  "Make a translation matrix from vector X Y Z."
  (let ((im (identity-matrix)))
    (setf (aref im 0 3) x
          (aref im 1 3) y
          (aref im 2 3) z)
    im))

(declaim (optimize (debug 3)))
(defun rotation-matrix (axis angle)
  "Make a matrix with rotation around AXIS with ANGLE degrees."
  (let ((im (identity-matrix))
        (r1)
        (r2)
        (s1 #'-)
        (s2 #'+))
    (iflet (symbolp axis)
        ((fn #'eq #'=)
         (val '(:x :y :z) '(0 1 2)))
      (cond ((funcall fn axis (car val))   (setq r1 1 r2 2))
            ((funcall fn axis (cadr val))  (setq r1 0 r2 2 s1 #'+ s2 #'-))
            ((funcall fn axis (caddr val)) (setq r1 0 r2 1))))
    (setf (aref im r1 r1) (cosd angle)
          (aref im r1 r2) (funcall s1 (sind angle))
          (aref im r2 r1) (funcall s2 (sind angle))
          (aref im r2 r2) (cosd angle))
    im))

(defun split-matrix (mat &optional translation-first)
  "Split the matrix MAT to rotation and translation matrices.
Return two values: the rotation and translation matrices.  If
TRANSLATION-FIRST is non-nil return the translation matrix as the
first value."
  (let ((rm (copy-array mat))
        (tm (translation-matrix (aref mat 0 3)
                                (aref mat 1 3)
                                (aref mat 2 3))))
    (setf (aref rm 0 3) 0 (aref rm 1 3) 0 (aref rm 2 3) 0)
    (if translation-first
        (values tm rm)
        (values rm tm))))

(defun matrix-locat (mat)
  "Make a location from translation components of MAT."
  (locat (aref mat 0 3) (aref mat 1 3) (aref mat 2 3)))
