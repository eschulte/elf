;;; elf-test.lisp --- Tests for elf.lisp

;; to run this test suite, load this file and evaluate (elf-test)

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
(in-package #:cl-user)
(require 'elf)
(require 'stefil)
(require 'alexandria)
(require 'trivial-shell)
(in-package #:elf)
(use-package 'alexandria)
(use-package 'trivial-shell)

(stefil:defsuite elf-test)
(stefil:in-suite elf-test)


;;; generic forensic functions over arbitrary objects
(defun show-it (hd &optional out)
  "Print the fields of a elf, section or program header.
Optional argument OUT specifies an output stream."
  (format (or out t) "~&")
  (mapcar
   (lambda (slot)
     (let ((val (slot-value hd slot)))
       (format (or out t) "~s:~a " slot val)
       (list slot val)))
   (mapcar #'my-slot-definition-name (my-class-slots (class-of hd)))))

(defun equal-it (obj1 obj2 &optional trace)
  "Equal over objects and lists."
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((or (and (listp obj1) (listp obj2)) (and (vectorp obj1) (vectorp obj2)))
       (and (equal (length obj1) (length obj2))
            (reduce (lambda (acc pair)
                      (and acc (equal-it (car pair) (cdr pair) trace1)))
                    (if (vectorp obj1)
                        (mapcar #'cons (coerce obj1 'list) (coerce obj2 'list))
                        (mapcar #'cons obj1 obj2))
                    :initial-value t)))
      ((my-class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (equal-it (slot-value obj1 slot) (slot-value obj2 slot)
                                    trace1)))
               (mapcar #'my-slot-definition-name
                       (my-class-slots (class-of obj1)))
               :initial-value t))
      (t (equal obj1 obj2)))))

(defun different-it (obj1 obj2 &optional trace)
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((or (and (vectorp obj1) (vectorp obj2))
           (and (proper-list-p obj1) (proper-list-p obj2)))
       (and (or (equal (length obj1) (length obj2))
                (format t "~&different lengths ~a!=~a"
                        (length obj1) (length obj2)))
            (reduce (lambda-bind (acc (i (a b)))
                      (and acc (or (different-it a b trace1)
                                   (format t "~& at ~d ~a!=~a" i a b))))
                    (indexed
                     (if (vectorp obj1)
                         (mapcar #'list (coerce obj1 'list) (coerce obj2 'list))
                         (mapcar #'list obj1 obj2)))
                    :initial-value t)))
      ((and (consp obj1) (consp obj2))
       (and (different-it (car obj1) (car obj2))
            (different-it (cdr obj1) (cdr obj2))))
      ((my-class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (or (different-it
                               (slot-value obj1 slot) (slot-value obj2 slot)
                               trace1)
                              (format t "~&  ~a" slot))))
               (mapcar #'my-slot-definition-name
                       (my-class-slots (class-of obj1)))
               :initial-value t))
      (t (or (equal obj1 obj2) (format t "~&~a!=~a" obj1 obj2))))))


;;; testing components
(defvar *test-class* :32-bit "architecture to test (e.g. 32-bit or 64-bit)")
(defvar *tmp-file* nil "temporary file used for testing")
(defvar *elf* nil "variable to hold elf object")

(stefil:defixture hello-elf
  (:setup
   (setf *tmp-file* "./hello.tmp")
   (when (probe-file *tmp-file*) (delete-file *tmp-file*))
   (setf *elf* (read-elf (case *test-class* (:32-bit "hello32") (:64-bit "hello64")))))
  (:teardown
   (when (probe-file *tmp-file*)
     (delete-file *tmp-file*))))

(stefil:deftest test-write-elf (elf path)
  (stefil:is (progn (write-elf elf path) (probe-file path))))

(stefil:deftest test-read-elf (path)
  (let (out)
    (stefil:is (setf out (read-elf path)))
    out))

(defun run-elf-tests ()
  (let ((*test-class* :32-bit)) (elf-test))
  (let ((*test-class* :64-bit)) (elf-test)))


;;; tests which run
(stefil:deftest test-magic-number ()
  (stefil:with-fixture hello-elf
    (let ((magic-number (concatenate 'string (string (code-char #x7f)) "ELF")))
      (stefil:is (equal magic-number (magic-number (header *elf*)))))))

(stefil:deftest test-idempotent-read-write ()
  (stefil:with-fixture hello-elf
    (stefil:is (equal *test-class* *class*))
    (test-write-elf *elf* *tmp-file*)
    (stefil:is (equal-it *elf* (test-read-elf *tmp-file*)))))

(stefil:deftest test-write-working-executable ()
  (stefil:with-fixture hello-elf
    (test-write-elf *elf* *tmp-file*)
    (stefil:is (probe-file *tmp-file*))
    (stefil:is #+ccl t ;; can't run this shell in ccl
               #-ccl (progn
                       (shell-command (format "chmod +x ~a" *tmp-file*))
                       (equal "hello world" (car (shell-command *tmp-file*)))))))

(stefil:deftest test-tweaked-text-working-executable ()
  (stefil:with-fixture hello-elf
    ;; change a `noop' which is not on the execution path to a `ret'
    (setf (aref (data (named-section *elf* ".text")) 42) #xc3)
    (test-write-elf *elf* *tmp-file*)
    (stefil:is (probe-file *tmp-file*))
    (stefil:is #+ccl t ;; can't run this shell in ccl
               #-ccl (progn
                       (shell-command (format "chmod +x ~a" *tmp-file*))
                       (equal "hello world" (car (shell-command *tmp-file*)))))))

;;; elf-test.lisp ends here
