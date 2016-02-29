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

(defpackage #:cleven
  (:use #:common-lisp)
  (:import-from #:alexandria
                #:ensure-cons
                #:mappend
                #:make-keyword
                #:make-gensym
                #:with-gensyms
                #:symbolicate
                #:read-file-into-string
                #:read-file-into-byte-vector
                #:write-byte-vector-into-file
                #:unwind-protect-case
                #:clamp
                #:copy-array)
  (:import-from #:cffi
                #:define-foreign-library
                #:use-foreign-library
                #:defcenum
                #:defcfun
                #:defctype
                #:null-pointer
                #:null-pointer-p
                #:inc-pointer
                #:incf-pointer
                #:pointer-eq
                #:foreign-alloc
                #:foreign-free
                #:foreign-pointer
                #:foreign-funcall
                #:mem-ref
                #:mem-aref
                #:with-foreign-object
                #:with-foreign-objects)
  (:import-from #:trivial-channels
                #:make-channel
                #:getmsg
                #:sendmsg)
  (:import-from #:cl-fad
                #:file-exists-p
                #:pathname-as-directory
                #:pathname-directory-pathname)
  (:import-from #:sb-cga
                #:vec+
                #:vec-
                #:vec=
                #:vec-min
                #:vec-max
                #:copy-vec
                #:matrix
                #:matrix*
                #:identity-matrix
                #:mref
                #:rotate
                #:translate
                #:transform-point)
  (:import-from #:sdl2
                #:*main-thread-channel*)
  (:export
   ;; Utils
   #:group
   #:it
   #:aif
   #:awhen
   #:mapa-b
   #:map0-n
   #:map1-n
   #:cons-if
   #:cons-if*
   #:unless*
   #:while
   #:for
   #:cat
   #:read-file
   #:write-file
   #:file-size
   #:iflet
   #:iflet*
   #:pathname-eq
   #:clamp-min
   #:clamp-max
   #:dir+file
   #:ceil
   #:add-hook
   #:remove-hook
   #:lock
   #:lock*
   #:with-lock
   ;; Location
   #:locat
   #:locatn
   #:locat+
   #:locat-
   #:locat*
   #:locat/
   ;; Game
   #:*event-loop-functions*
   #:*event-loop-functions-lock*
   #:event-case
   #:*game-init-functions*
   #:*game-init-functions-lock*
   #:*game-quit-hook*
   #:*game-quit-hook-lock*
   #:run-game
   #:quit-game
   #:wob
   #:make-global-world
   ;; Sprite
   #:make-sprite
   ;; Camera
   #:*camera-zoom*
   #:*camera-tilt*
   #:*camera-azimuth*
   #:*camera-location*
   ;; Render
   #:*render-init-hook*
   #:*render-init-hook-lock*
   #:*render-loop-hook*
   #:*render-loop-hook-lock*
   #:*render-quit-hook*
   #:*render-quit-hook-lock*
   #:render-loop))
