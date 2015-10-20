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

(defpackage #:cleven-sandbox
  (:nicknames #:sandbox)
  (:use #:common-lisp #:cleven)
  (:import-from #:alexandria
                #:clamp))

(in-package #:sandbox)

(defun run-sandbox ()
  (add-hook '*event-loop-functions* 'events-hook)
  (add-hook '*render-init-hook* 'init-render)
  (add-hook '*game-init-functions* 'render-loop)
  (run-game sdl2-ffi:+sdl-init-video+ :win-w 600 :win-x 766 :win-y 0))

;;; FIXME If we got an error upon quit next sandbox running hangs
;;; somewhere in `sdl2:in-main-thread'.
(defun quit-sandbox ()
  (quit-game)
  (remove-hook '*render-init-hook* 'init-render)
  (remove-hook '*game-init-functions* 'render-loop)
  (remove-hook '*event-loop-functions* 'events-hook))

(defun events-hook (event)
  (event-case event
    (:keydown
     (:keysym keysym)
     ;; TODO I need more compact event handling
     (cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
            (quit-sandbox))
           ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
            (decf *camera-azimuth* 5))
           ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
            (incf *camera-azimuth* 5))
           ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
            (incf *camera-tilt* 5))
           ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
            (decf *camera-tilt* 5))
           ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-equals)
            (incf *camera-zoom* (if (< *camera-zoom* 1) .1 1)))
           ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-minus)
            (setf *camera-zoom* (if (<= *camera-zoom* 1)
                                    (clamp (- *camera-zoom* .1) .1 1)
                                    (if (< (- *camera-zoom* 1) 1)
                                        1
                                        (1- *camera-zoom*)))))))))

(defun init-render ()
  (gl:clear-color 0.1 0.1 0.1 1.0))
