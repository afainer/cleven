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

(defvar *foreign-types-expanders*
  (list
   (cons 'scalar
         #'(lambda (val)
             `(:float (float ,val 0f0))))
   (cons 'vec
         #'(lambda (val)
             `(scalars ((vecx ,val) (vecy ,val) (vecz ,val)))))
   ;; The upper left 3x3 submatrix
   (cons 'mat3
         #'(lambda (val)
             `(scalars ((mref ,val 0 0) (mref ,val 0 1) (mref ,val 0 2)
                        (mref ,val 1 0) (mref ,val 1 1) (mref ,val 1 2)
                        (mref ,val 2 0) (mref ,val 2 1) (mref ,val 2 2))))))
  "Expander functions used by `expand-foreign-type'.")

;;; Plural types may be ordinary expanders in *foreign-types-expanders*
(defvar *plural-foreign-types*
  (nconc (mapcar #'(lambda (type)
                     (cons (make-keyword (cat (symbol-name type)
                                              (symbol-name 's)))
                           type))
                 (append cffi:*built-in-integer-types*
                         cffi:*built-in-float-types*
                         (remove :void cffi:*other-builtin-types*)))
         (mapcar #'(lambda (type)
                     (cons (symbolicate type 's) type))
                 (list 'scalar 'vec 'mat3)))
  "Set of foreign types and their plural names, e.g.
 ((:int . :ints) (:float . :floats) ...).")

(defvar *with-foreign-names* nil
  "Names of with-foreign-* macros defined by `define-with-foreign'.
Each element of this list is a cons (name . %name), where name is a
name of front-end macro and %name is low-level macro.")

(defun expand-foreign-type (expanders type arg)
  "Expand TYPE into CFFI built-in type using EXPANDERS.
If TYPE is plural then ARG must be a list, e.g. :ints (a b c) is
expanded to a list of pairs: (:int a :int b :int c).

EXPANDERS is a list of conses (type . expander-function).  Find cons
if type `eq' TYPE and call expander-function with ARG.  If its value
is nil, find another cons.  Otherwise recursively expand the value.
If all expander functions return nil or there is not any cons with
TYPE return (list TYPE ARG)."
  (aif (assoc type *plural-foreign-types*)
       (loop
          for a in arg
          nconc (expand-foreign-type expanders (cdr it) a))
       (let ((exp expanders)
             (r (list type arg)))
         (while (setq exp (member type exp :key #'car))
           (aif (funcall (cdar exp) arg)
                (return (setq r (apply #'expand-foreign-type expanders it)))
                (pop exp)))
         r)))

(defmacro %cfuncall (name-and-options expanders &rest args)
  `(foreign-funcall
    ,name-and-options
    ,@(loop
         with eargs
         for (type arg) on args by #'cddr
         unless arg
         do
           (setq eargs (nconc eargs (list type)))
         else do
           (setq eargs (nconc eargs (expand-foreign-type expanders type arg)))
         finally (return eargs))))

(defmacro cfuncall (name-and-options &rest args)
  "Perform a foreign function call.
The macro is a wrapper around `foreign-funcall' that expands ARGS
using `expand-foreign-type'."
  `(%cfuncall ,name-and-options ,*foreign-types-expanders* ,@args))

(defun make-macrobindings (expanders vars)
  "Make macrolet for each macro name in `*with-foreign-names*'."
  (flet ((cons-expander (vars)
           `(cons
             (cons
              :pointer
              #'(lambda (arg)
                  (cond
                    ,@(loop
                         for (v . g) in vars
                         collect `((eq arg ',v)
                                   ,(if (cdr g)
                                        `'(:pointers ,g)
                                        `'(:pointer ,(car g))))))))
             ',expanders)))
    `((cfuncall (name-and-options &rest args)
                `(%cfuncall ,name-and-options
                            ,,(cons-expander vars)
                            ,@args))
      ,@(loop
           for (name %name) in *with-foreign-names*
           collect `(,name (bindings &body body)
                           `(,',%name ,bindings
                                      ,,(cons-expander vars)
                                      ,@body))))))

(defun make-accessors (expanders type vars make-type body)
  "Make accessors for each variable in VARS."
  (let ((acs (mapcar #'(lambda (v)
                         (make-gensym (car v)))
                     vars))
        (exp (group (expand-foreign-type expanders
                                         type
                                         'new)
                    2)))
    (flet ((mkmemrefs (var)
             (mapcar #'(lambda (gv tp)
                         `(mem-ref ,gv ,tp))
                     (cdr var)
                     (mapcar #'car exp))))
      `(flet ,(mapcan #'(lambda (a v)
                          (list
                           `(,a ()      ;Accessor
                                ,(if make-type
                                     `(,make-type ,@(mkmemrefs v))
                                     (car (mkmemrefs v))))
                           `((setf ,a) (new) ;Setter
                             (setf ,@(mapcan #'(lambda (mem val)
                                                 (list mem val))
                                             (mkmemrefs v)
                                             (mapcar #'cadr exp)))
                             new)))
                      acs vars)
         (declare (ignorable ,@(mapcan #'(lambda (a)
                                           (list `(function ,a)
                                                 `(function (setf ,a))))
                                       acs)))
         (symbol-macrolet ,(mapcar #'(lambda (v a)
                                       `(,(car v) (,a)))
                                   vars acs)
           ,@body)))))

(defmacro define-with-foreign (name %name type make-type)
  "Define with-foreign- macro NAME for TYPE.

The with-foreign- macro is a macro

  with-foreign-TYPEs bindings &body body

where bindings is a list similar to bindings for `let' special form.

For example:

  (with-foreign-vecs (vec)
    (cfuncall \"get_gravity\" :pointers (*physics* loc))
    loc)

This macro call creates a new vector VEC which is used as a pointer
to the foreign function `get_gravity'.

Note that actual name of defined macro is NAME which is created by the
macrolet after `define-with-foreign' definition.  NAME macro is a
wrapper around %NAME macro which is also defined by
`define-with-foreign'.  The definition of the macro %NAME is

  %NAME bindings expanders &body

where expanders is a list of type expanders for `expand-foreign-type'.

%NAME does the following:

  - Makes foreign object for each component of TYPE.
  - Generates macrolets that rebinds cfuncall all macros in
    `*with-foreign-names*'.
  - Generates accessors for bound variables, so expressions like this
    work correctly: (list v (setf v (vec 1 2 3)))

Each macro rebound by `macrolet' special form is a wrapper which
generates new type expanders end expands to corresponding macro,
e.g. with-foreign-vecs to %with-foreign-vecs.  The new expanders
translate each `cfuncall' :pointer argument from symbols in the
bindings to the foreign objects.

Symbol %NAME is created by the macrolet after `define-with-foreign'
definition."
  `(prog1
       (defmacro ,name (bindings &body body)
         `(,',%name ,bindings ,*foreign-types-expanders* ,@body))
     (defmacro ,%name (bindings expanders &body body)
       (let* ((bindings (mapcar #'(lambda (b)
                                    (if (and (consp b) (cdr b))
                                        `(,(car b) ,(cadr b))
                                        (ensure-cons b)))
                                bindings))
              ;; Expanded foreign types
              (ftypes (mapcar #'car
                              (group (expand-foreign-type expanders
                                                          ',type
                                                          nil)
                                     2)))
              ;; Each variable have a list of foreign variables
              (vars (mapcar #'(lambda (b)
                                (cons (car b)
                                      (map1-n #'(lambda (n)
                                                  (declare (ignore n))
                                                  (make-gensym (car b)))
                                              (length ftypes))))
                            bindings)))
         `(with-foreign-objects
              ;; Generate foreign variables
              ,(mapcan #'(lambda (v)
                           (mapcar #'(lambda (gv tp) `(,gv ',tp))
                                   (cdr v)
                                   ftypes))
                       vars)
            (macrolet ,(make-macrobindings expanders vars)
              ,(make-accessors expanders ',type vars ',make-type
                               `(,@(loop
                                      for b in bindings
                                      if (cdr b) collect `(setf ,@b))
                                   ,@body))))))))

(macrolet
    ((define-types (&rest type-mktype)
       (let (names types mktypes)
         (mapc
          #'(lambda (tm)
              (destructuring-bind (type mktype) tm
                (let* ((name (symbolicate 'with-foreign- type 's)))
                  (push (list name (symbolicate '% name)) names)
                  (push type types)
                  (push mktype mktypes))))
          type-mktype)
         `(progn
            (setq *with-foreign-names*
                  (list ,@(mapcar #'(lambda (n)
                                      `(list ',(car n) ',(cadr n)))
                                  names)))
            ,@(nreverse
               (mapcar #'(lambda (name type mktype)
                           `(define-with-foreign ,@name ,type ,mktype))
                       names
                       types
                       mktypes))))))
  (define-types
      (scalar nil)
      (vec vec)
      (mat3 matrix3)))
