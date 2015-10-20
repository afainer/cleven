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

;;; Unix specific definitions
#+unix
(progn
  (defctype size-t :unsigned-int)
  (defctype off-t :long)

  (defcenum copen-flags
    (:rdonly 0)
    (:wronly 1)
    (:rdwr 2))

  (defcfun ("open" copen) :int
    (pathname :string)
    (flags copen-flags))

  (defcfun ("close" cclose) :int
    (fd :int))

  (defcenum mmap-prot
    (:read 1)
    (:write 2)
    (:rdwr 3))

  (defcenum mmap-flags
    (:shared 1)
    (:private 2))

  (defvar mmap-failed (cffi:inc-pointer (null-pointer) -1)
    "The return value of `mmap' if it failed to map a file.")

  (defcfun "mmap" :pointer
    (addr :pointer)
    (length size-t)
    (prot mmap-prot)
    (flags mmap-flags)
    (fd :int)
    (offset off-t))

  (defcfun "munmap" :int
    (addr :pointer)
    (length size-t)))
;;; End of Unix definitions

(defstruct mmapped
  "Memory mapped file struct."
  (fd 0 :read-only t)
  (addr (cffi:null-pointer) :read-only t)
  (length 0 :read-only t))

(defun mmap-cleanup (fd addr length)
  "Delete mapping for ADDR and close FD."
  (unless (cffi:pointer-eq addr mmap-failed)
    (munmap addr length))
  (unless (< fd 0)
    (cclose fd)))

(defun mmap-file (filename &optional shared)
  "Memory map the file FILENAME.
If SHARED is non-`nil' then updates to the mapping are visible to
other processes that map this file, and are carried through to the
underlying file."
  (let ((length (file-size filename))
        (fd (copen (namestring filename) :rdwr))
        (addr mmap-failed))
    (if (< fd 0)
        (error "Could not to open a file: ~A" filename)
        (unwind-protect-case ()
            (if (cffi:pointer-eq mmap-failed
                                 (setq addr
                                       (mmap (cffi:null-pointer)
                                             length
                                             :rdwr
                                             (if shared :shared :private)
                                             fd 0)))
                (error "Could not to memory map file: ~A" filename)
                (make-mmapped :fd fd :addr addr :length length))
          (:abort (mmap-cleanup fd addr length))))))

(defun munmap-file (mapped)
  "Delete the mapping MAPPED."
  (mmap-cleanup (mmapped-fd mapped)
                (mmapped-addr mapped)
                (mmapped-length mapped)))
