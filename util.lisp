;;; util.lisp --- utility functions for elf.lisp

;; For information on the elf format see the following
;; http://www.muppetlabs.com/~breadbox/software/ELF.txt

;; Copyright (C) 2011  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(in-package #:elf)

(defun range (n &optional m)
  "Return the numbers in range."
  (funcall (if (and m (< m n))
               (prog1 #'nreverse (let ((o n)) (setf n m) (setf m o)))
               #'identity)
           (loop for num from (if m n 0) to (if m m (- n 1)) collect num)))

(defun indexed (sequence)
  (loop for el in sequence as n from 0
     collect (list n el)))

(defun subseq-of (sub seq &key (start 0))
  "If SUB is a occurs in SEQ, return the index at which is appears."
  (block nil
    (mapcon (lambda (seq)
              (when (starts-with-subseq sub seq)
                (return start))
              (incf start))
            (coerce (subseq seq start) 'list))
    (return nil)))

;; The following two functions are adapted from:
;; http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings
(def-memoized-function lcs (l1 l2 &key (test #'eql))
  "Return the longest common sub-list of L1 and L2 using TEST."
  (when (not (or (null l1) (null l2)))
    (if (funcall test (car l1) (car l2))
        (cons (car l1) (lcs (cdr l1) (cdr l2) :test test))
        (first (sort (list (lcs l1 (cdr l2) :test test)
                           (lcs (cdr l1) l2 :test test))
                     #'> :key #'length)))))

(defun diff (list1 list2 &key (test #'eql))
  "Find the differences between LIST1 and LIST2 using TEST."
  (let ((lcs (lcs list1 list2 :test test))
        result)
    (dolist (c lcs)
      (let* ((sync-list1 (position c list1 :test test))
             (sync-list2 (position c list2 :test test))
             (removed (subseq list1 0 sync-list1))
             (added (subseq list2 0 sync-list2)))
        (setf list1 (subseq list1 (1+ sync-list1)))
        (setf list2 (subseq list2 (1+ sync-list2)))
        (when removed
          (push (cons :removed removed) result))
        (when added
          (push (cons :added added) result))
        (push c result)))
    (when list1
      (push (cons :removed list1) result))
    (when list2
      (push (cons :added list2) result))
    (nreverse result)))

(defun deltas (list1 list2 &key (test #'eql) &aux (delta 0))
  "Return a list of index offsets for the elements of LIST1."
  (nreverse
   (reduce
    (lambda (acc el)
      (if (listp el)
          (case (first el)
            (:added   (prog1 acc
                        (setf delta (+ delta (length (cdr el))))))
            (:removed (prog1 (append (mapcar
                                      (lambda (el) (declare (ignorable el)) delta)
                                      (cdr el))
                                     acc)
                        (setf delta (- delta (length (cdr el)))))))
          (cons delta acc)))
    (diff list1 list2 :test test) :initial-value nil)))

(defun my-slot-definition-name (el)
  #+sbcl
  (sb-mop::slot-definition-name el)
  #+ccl
  (ccl:slot-definition-name el)
  #-(or sbcl ccl)
  (#'clos::slot-definition-name el))

(defun my-class-slots (el)
  #+sbcl
  (sb-mop::class-slots el)
  #+ccl
  (ccl:class-slots el)
  #-(or sbcl ccl)
  (clos::class-slots el))

(defun mapslots (func obj)
  "Map func over the slots of the clos object OBJ."
  (mapcar func
          (mapcar #'my-slot-definition-name
                  (my-class-slots (class-of obj)))))

(defun generic-copy (obj &optional trace)
  "A generic copy method, may run way too long on partially circular elements."
  (let ((trace1 (concatenate 'list (list obj) trace)))
    (cond
      ((or (numberp obj) (symbolp obj)) obj)
      ((stringp obj) (copy-seq obj))
      ((member obj trace) obj)      ; don't follow circular structures
      ((or (listp obj) (vectorp obj))
       (coerce (mapcar (lambda (el) (generic-copy el trace1)) (coerce obj 'list))
               (cond ((listp obj) 'list) ((vectorp obj) 'vector))))
      ((my-class-slots (class-of obj))
       (let ((new (make-instance (class-name (class-of obj)))))
         (mapslots
          (lambda (slot) (setf (slot-value new slot)
                          (generic-copy (slot-value obj slot) trace1)))
          obj)
         new))
      (t (error "~&don't know how to copy ~a" obj)))))

(defun temp-file-name ()
  #+clisp
  (let ((stream (gensym)))
    (eval `(with-open-stream (,stream (ext:mkstemp nil))
             (pathname ,stream))))
  #+sbcl
  (swank-backend::temp-file-name)
  #+ccl
  (ccl:temp-pathname)
  #-(or sbcl clisp ccl)
  (error "no temporary file backend for this lisp."))

(defun trim (str &key (chars '(#\Space #\Tab)))
  (loop until (or (emptyp str) (not (member (aref str 0) chars)))
     do (setf str (subseq str 1)))
  (loop until (or (emptyp str) (not (member (aref str (1- (length str))) chars)))
     do (setf str (subseq str 0 (1- (length str)))))
  str)

(defmacro lambda-registers (registers regexp &body body)
  "Create a function over the register matches using `register-groups-bind'."
  (with-gensyms (string)
    `(lambda (,string)
       (register-groups-bind ,registers (,regexp ,string)
         ,@body))))

;;; util.lisp ends here
