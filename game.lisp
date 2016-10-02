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

;;; TODO Make a macro which defines the hook and its lock
(defvar *event-loop-functions* ()
  "List of functions to be called for each received event.
Functions are called on one argument, the received event.")

(defvar *event-loop-functions-lock* (make-shared '*event-loop-functions*)
  "The shared object for `*event-loop-functions*'.")

;;; FIXME If we quit-game from code, the event-loop do not exit becase
;;; it still waiting events.
(defun event-loop ()
  "Run the game event loop in the main thread.
For each received event run functions in `*event-loop-functions*'.  If
the event is :quit exit from the event loop."
  (sdl2:in-main-thread ()
    (sdl2:with-sdl-event (event)
      (loop until (eq :quit (getmsg *main-thread-channel*))
         do
           (when (/= 0 (sdl2:next-event event :wait))
             (lock #'run-hook-with-args *event-loop-functions-lock* event))))))

(defmacro event-case (event &body handlers)
  "SDL2-style event handling.
See `sdl2:with-event-loop'."
  (with-gensyms (gevent type)
    ;; At the moment ignore user data
    `(let* ((,gevent ,event)
            (,type (sdl2:get-event-type ,gevent)))
       (case ,type
         ,@(loop :for (type params . forms) :in handlers
              ;; XXX sdl2-style event handling is used just for the
              ;; first prototype
              :collect (sdl2::expand-handler event type params forms)
              :into results
              :finally (return (remove nil results)))))))

(defmacro with-game-init (sdl-init-flags &body body)
  "Initialize game with SDL-INIT-FLAGS and evaluate forms of BODY.
See `sdl2:init'."
  (let ((gflags (gensym)))
    `(let ((,gflags ,sdl-init-flags))
       (apply #'sdl2:init (if (listp ,gflags)
                              ,gflags
                              (list ,gflags)))
       (unwind-protect
            (sdl2:in-main-thread () ,@body)
         (sdl2:quit)))))

(defvar *game-init-functions* ()
  "List of functions to be called before the game is started.
Each function is called on one argument, the current game window.")

(defvar *game-init-functions-lock* (make-shared '*game-init-functions*)
  "The shared object for `*game-init-functions*'.")

(defvar *game-quit-hook* ()
  "Hook run after game quits.")

(defvar *game-quit-hook-lock* (make-shared '*game-quit-hook*)
  "The shared object for `*game-quit-hook*'.")

(defun run-game (sdl-init-flags
                 &key (win-title "Cleven game")
                   (win-x :centered) (win-y :centered)
                   (win-w 800) (win-h 600)
                   (win-flags (list :shown :opengl)))
  "Run game with SDL-INIT-FLAGS and optional window parameters."
  (with-game-init sdl-init-flags
    ;; TODO It would be nice to have ability to create multiple
    ;; windows
    (sdl2:with-window (win
                       :title win-title
                       :x win-x :y win-y
                       :w win-w :h win-h
                       :flags win-flags)
      (unwind-protect
           (progn
             (lock #'run-hook-with-args *game-init-functions-lock* win)
             (event-loop))
        (quit-all-threads)
        ;; It is save to run quit hooks now without locking
        (run-hooks '*game-quit-hook*)))))

(defun quit-game ()
  "Quit game sending :quit message to the main thread."
  (sendmsg *main-thread-channel* :quit))

(defstruct (wob (:constructor %make-wob))
  "The world object structure.
The structure is defined for `wob' type and its `print-object'
method."
  operations)

(defun make-wob (name &rest operations)
  "Make a world object with NAME and OPERATIONS.
World object is object which is placed in the game world at specified
locations.  Sprites put them in `*world*' and the renderer read them.

NAME is the name of the created object.

OPERATIONS is a list with even number of elements

\(OPNAME1 OPFN1 OPNAME2 OPFN2 ...).

OPNAME<n> is used to refer to OPFN<2>.  Is it an error to specify
operation named :NAME.  The object operations are closures.  To call
particular operation of an world object use the function `wob'."
  ;; TODO Add local before/after operation hooks
  (if (oddp (length operations))
      (error "Odd number of operations."))
  (let ((operats (make-hash-table)))
    (setf (gethash :name operats) #'(lambda (&rest args)
                                      (declare (ignore args))
                                      name))
    (dolist (op (group operations 2))
      (if (eq (car op) :name)
          (error "Wrong name of an operation: ~A." :name))
      (setf (gethash (car op) operats) (cadr op)))
    (%make-wob :operations
               #'(lambda (operat &rest args)
                   (apply (gethash operat operats
                                   ;; Maybe return two values as `gethash'
                                   ;; does
                                   #'(lambda (&rest args)
                                       (declare (ignore args))))
                          args)))))

(declaim (inline wob))
(defun wob (op ob &rest args)
  "Apply the operation OP on the world object OB and ARGS."
  (apply (wob-operations ob) op args))

(defmethod print-object ((ob wob) stream)
  (print-unreadable-object (ob stream :type t)
    (format stream "~A, " (wob :name ob))
    (wob :print ob stream)))

;;; FIXME Use *world-lock*
(defvar *world* nil
  "The game world.
For simplicity, world is just a plain list.")

(defun add-to-world (wob)
  "Add WOB to world."
  (pushnew wob *world*))

(defun remove-from-world (wob)
  "Remove WOB from world."
  (with-world-tile (tile )
    (delete wob *world*)))
