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

(defun make-shader (type shader)
  "Compile SHADER of TYPE."
  (let ((sh (gl:create-shader type)))
    (if (= sh 0)
        (error "A shader of type ~A is not created" type))
    (gl:shader-source sh shader)
    (gl:compile-shader sh)
    (unless (gl:get-shader sh :compile-status)
      (error "Shader compilation failed.  The info log is:~%~A"
             (gl:get-shader-info-log sh)))
    sh))

(defun use-shaders (vertex-shader fragment-shader &rest attrib-locations)
  "Make a shader program from VERTEX-SHADER and FRAGMENT-SHADER.
VERTEX-SHADER and FRAGMENT-SHADER are strings with shaders sources."
  (let ((pr (gl:create-program)))
    (if (= pr 0)
        (error "A program is not created"))
    (gl:attach-shader pr (make-shader :vertex-shader vertex-shader))
    (gl:attach-shader pr (make-shader :fragment-shader fragment-shader))
    (dolist (a attrib-locations)
      (gl:bind-attrib-location pr (car a) (cdr a)))
    (gl:link-program pr)
    (unless (gl:get-program pr :link-status)
      (error "Program linking failed.  The info log is:~%~A"
             (gl:get-program-info-log pr)))
    (gl:use-program pr)
    pr))

(defun free-shaders (program)
  "Delete the shader program PROGRAM."
  (mapc #'gl:delete-shader (gl:get-attached-shaders program))
  (gl:delete-program program))
