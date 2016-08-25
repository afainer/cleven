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

(defvar *game-threads* (make-shared (make-hash-table :test 'eq))
  "The hash with threads of the game.
The key is the thread object. The value is its channel.")

(defun add-thread (thread channel)
  "Add THREAD with CHANNEL to the game."
  (with-lock (gt *game-threads*)
    (setf (gethash thread gt) channel)))

(defun add-current-thread ()
  "Add the current thread to the game."
  (add-thread (bt:current-thread)
              (make-channel)))

(defun remove-thread (thread)
  "Remove THREAD from the game."
  (lock* #'remhash thread *game-threads*))

(defun sendmsg-to-thread (thread msg)
  "Send MSG to THREAD."
  (with-lock (gt *game-threads*)
    (aif (gethash thread gt)
         (sendmsg it msg))))

(defun quit-thread (thread &optional (wait t))
  "Send THREAD the quit message.
If WAIT is non-nil, wait for THREAD quitting."
  (sendmsg-to-thread thread :quit)
  (when wait
      (while (lock* #'gethash thread *game-threads*))))

(defun quit-all-threads (&optional (wait t))
  "Send the quit message to all game threads."
  (with-lock (gt *game-threads*)
    (maphash #'(lambda (thread channel)
                 (declare (ignore thread))
                 (sendmsg channel :quit))
             gt))
  (when wait
    (while (< 0 (lock #'hash-table-count *game-threads*)))))

(defmacro make-game-thread ((&optional name)
                            &body body)
  "Make and run a game thread with optional NAME.
The new thread exits after the last form of BODY.  In scope of forms
of BODY the local macro `thread-loop' is defined.

thread-loop form*

Forms of the `thread-loop' evaluated sequentially in implicit loop
until the current thread receives the quit message.  See
`quit-thread'.  Also, each received message of type function is
`funcall'ed."
  (let ((channel (gensym)))
    `(bt:make-thread
      #'(lambda ()
          (macrolet ((thread-loop (&body b)
                       (let ((msg (gensym)))
                         `(loop
                             with ,msg
                             ;; TODO It is better to make a quit
                             ;; function which shares the quit lexical
                             ;; variable with the loop until
                             ;; construct.
                             until (eq :quit (setq ,msg (getmsg ,',channel)))
                             do (when (functionp ,msg) (funcall ,msg))
                               ,@b))))
            (unwind-protect
                 (let ((,channel (add-current-thread)))
                   ,@body)
              (remove-thread (bt:current-thread)))))
      :name ,name)))

(defmacro tlambda (lambda-list thread &body body)
  "Make a function that is local to THREAD.
The thread local function is a function which executes in its thread
loop.  Return value is always nil."
  ;; TODO The return value should be received via a temporarily channel.
  ;; For example, send the cons (thread . temp-channel) to the thread.
  (with-gensyms (gthr args)
    `#'(lambda (&rest ,args)
         (let ((,gthr ,thread)
               (fn #'(lambda ()
                       (destructuring-bind ,lambda-list ,args
                         ,@body))))
           (if (eq ,gthr (bt:current-thread))
               (funcall fn)
               (sendmsg-to-thread ,gthr fn))
           nil))))

;;; TODO Definition of thread functions requires their threads are
;;; already created.  It is better to referer to threads using names,
;;; e.g.:
;;;
;;; (defthreadfun reload-shaders (v f) 'render-thread...)
;;;
(defmacro defthreadfun (name lambda-list thread &body body)
  "Define a function named NAME that is local to THREAD.
If thread is nil then the function is local to the current thread."
  ;; TODO I want to see arguments using Autodoc mode
  (with-gensyms (args gthr)
    `(let ((,gthr ,(if thread
                       thread
                       '(bt:current-thread))))
       (defun ,name (&rest ,args)
         (apply (tlambda ,lambda-list ,gthr
                  ,@body)
                ,args)))))
