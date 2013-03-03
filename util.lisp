;;; util.lisp --- utility functions for elf.lisp

;; For information on the elf format see the following
;; http://www.muppetlabs.com/~breadbox/software/ELF.txt

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(use-package :alexandria)

(defun indexed (sequence)
  (loop for el in sequence as n from 0
     collect (list n el)))

#+sbcl
(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

(defun temp-file-name ()
  #+clisp
  (let ((stream (gensym)))
    (eval `(with-open-stream (,stream (ext:mkstemp nil))
             (pathname ,stream))))
  #+sbcl
  (tempnam nil nil)
  #+ccl
  (ccl:temp-pathname)
  #-(or sbcl clisp ccl)
  (error "no temporary file backend for this lisp."))

(defun trim (str &key (chars '(#\Space #\Tab #\Newline)))
  (loop until (or (emptyp str) (not (member (aref str 0) chars)))
     do (setf str (subseq str 1)))
  (loop until (or (emptyp str) (not (member (aref str (1- (length str))) chars)))
     do (setf str (subseq str 0 (1- (length str)))))
  str)
