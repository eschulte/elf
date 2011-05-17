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
(in-package #:elf)
(use-package 'stefil)

(defsuite elf-test)
(in-suite elf-test)


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

(defixture hello-elf
  (:setup
   (setf *tmp-file* "./hello.tmp")
   (when (probe-file *tmp-file*) (delete-file *tmp-file*))
   (setf *elf* (read-elf (case *test-class*
                           (:32-bit "hello32")
                           (:64-bit "hello64")))))
  (:teardown
   (when (probe-file *tmp-file*)
     (delete-file *tmp-file*))))

(deftest test-write-elf (elf path)
  (is (progn (write-elf elf path) (probe-file path))))

(deftest test-read-elf (path)
  (let (out)
    (is (setf out (read-elf path)))
    out))

(defun run-elf-tests ()
  (let ((*test-class* :32-bit)) (elf-test))
  (let ((*test-class* :64-bit)) (elf-test)))


;;; tests which run
(deftest test-magic-number ()
  (with-fixture hello-elf
    (let ((magic-number (concatenate 'string (string (code-char #x7f)) "ELF")))
      (is (equal magic-number (magic-number (header *elf*)))))))

(deftest test-section-sizes ()
  (with-fixture hello-elf
    (dolist (sec (sections *elf*))
      (unless (or (member (name sec) '(".dynsym" ".dynamic" ".symtab")
                          :test #'string=)
                  (member (type sec) '(:rel :rela)))
        (is (= (size (sh sec)) (length (data sec)))
            "section:~a data:~a != size:~a"
            (name sec) (length (data sec)) (size (sh sec)))))))

(deftest test-idempotent-read-write ()
  (with-fixture hello-elf
    (is (equal *test-class* *class*))
    (test-write-elf *elf* *tmp-file*)
    (is (equal-it *elf* (test-read-elf *tmp-file*)))))

(deftest test-write-working-executable ()
  (with-fixture hello-elf
    (test-write-elf *elf* *tmp-file*)
    (is (probe-file *tmp-file*))
    (is #+ccl t ;; can't run this shell in ccl
        #-ccl (progn
                (shell-command (format "chmod +x ~a" *tmp-file*))
                (equal "hello world" (car (shell-command *tmp-file*)))))))

(deftest test-tweaked-text-working-executable ()
  (with-fixture hello-elf
    ;; change a `noop' which is not on the execution path to a `ret'
    (setf (aref (data (named-section *elf* ".text")) 42) #xc3)
    (test-write-elf *elf* *tmp-file*)
    (is (probe-file *tmp-file*))
    (is #+ccl t ;; can't run this shell in ccl
        #-ccl (progn
                (shell-command (format "chmod +x ~a" *tmp-file*))
                (equal "hello world" (car (shell-command *tmp-file*)))))))

(deftest test-data-setf-changes ()
  (with-fixture hello-elf
    (let ((first-data (copy-seq (data (named-section *elf* ".text")))))
      (setf (aref (data (named-section *elf* ".text")) 42) #xc3)
      (is (not (equal-it first-data (data (named-section *elf* ".text"))))))))

(deftest test-pernicious-data-setf-changes ()
  (let ((bad #(49 237 73  137 209 94 72 137  226 72 131 228 240  80 84 73
               199 192 64 5 64 0 72 199 193  80 5 64 0 72 199 199 36 5 64
               0 232 191  255 255 255 244 144  144 72 131 236 8  72 139 5
               105 11 32 0 72 133 192 116  2 255 208 72 131 196 8 195 144
               144 144 144 144 144 144 144  144 144 144 144 144 85 72 137
               229 117 72 131 236 8 128 61 128 11 32 0 0 117 75 187 48 14
               96 0 72 139 5 122 11 32 0 72 129 235 40 14 96 0 72 193 251
               3 72 131  235 1 72 57 216 115  36 102 15 31 68 0  0 72 131
               192 1 72 137 5  85 11 32 0 255 20 197 40 14  96 0 72 139 5
               71 11 32 0 72 57 216 114 226 198 5 51 11 32 0 1 72 131 196
               8 91 201 195 102 102 102 46 15  31 132 0 0 0 0 0 85 72 131
               61 47 9  32 0 0 72 137 229  116 18 184 0 0 0  0 72 133 192
               116 8 191 56 14 96 0 201 255 224 201 195 144 144 85 72 137
               229 191 44 6 64 0 232 230  254 255 255 201 195 144 144 144
               144 144 144  144 144 144 144  144 144 243 195  102 102 102
               102 102 46  15 31 132 0 0 0  0 0 72 137 108 36  216 76 137
               100 36 224  72 141 45 179 8 32  0 76 141 37 172 8  32 0 76
               137 108 36 232 76 137 116 36  240 76 137 124 36 248 72 137
               92 36 208 72 131 236 56 76 41 229 65 137 253 73 137 246 72
               193 253 3 73 137 215 232 91  254 255 255 72 133 237 116 28
               49 219 15 31 64 0 76 137  250 76 137 246 68 137 239 65 255
               20 220 72  131 195 1 72 57  235 114 234 72 139 92  36 8 72
               139 108 36 16 76 139 100 36 24 76 139 108 36 32 76 139 116
               36 40 76 139  124 36 48 72 131 196 56 195  144 144 144 144
               144 144 144 85 72 137 229 83 72 131 236 8 72 139 5 40 8 32
               0 72 131 248 255 116 25 187 24 14 96 0 15 31 68 0 0 72 131
               235 8 255 208 72 139 3 72  131 248 255 83 241 72 131 196 8
               91 201 195 144 144))
        (*test-case* :64-bit))
    (with-fixture hello-elf
      (setf (data (named-section *elf* ".text")) bad))))

(deftest test-objdump ()
  (with-fixture hello-elf
    (is (stringp (objdump (named-section *elf* ".text"))))))

(deftest test-objdump-parse ()
  (with-fixture hello-elf
    (let ((sym-names (mapcar #'sym-name (symbols *elf*))))
      (mapc
       (lambda-bind (((value . name) . addrs))
         (is (numberp value))
         (is (stringp name))
         (is (listp addrs))
         (is (member name sym-names :test #'string=)))
       (objdump-parse (objdump (named-section *elf* ".text")))))))

(deftest test-objdump-parse-empty-instructions ()
  (let ((lines (with-open-file (in "main.txt")
                               (loop for line = (read-line in nil :eof)
                                  until (eq line :eof)
                                  collect line))))
    (is (equal-it '(0) (second (nth 12 (parse-addresses lines)))))))

(deftest test-equality-of-data-and-objdump-bytes ()
  (with-fixture hello-elf
    (let ((.text (named-section *elf* ".text")))
      (is (equal-it (coerce (data .text) 'list)
                    (apply #'append
                           (apply #'append
                                  (mapcar
                                   (lambda (sym) (mapcar #'second (second sym)))
                                   (objdump-parse (objdump .text))))))))))

(deftest test-addresses-compared-w/objdump-output ()
  (with-fixture hello-elf
    (let ((.text (named-section *elf* ".text")))
      (is (equal-it
           (apply #'append
                  (mapcar
                   (lambda-bind ((addr bytes disasm))
                     (declare (ignorable disasm))
                     (mapcar (lambda (_)
                               (declare (ignorable _))
                               (prog1 addr (incf addr))) bytes))
                   (apply #'append
                          (mapcar #'second
                                  (objdump-parse
                                   (objdump (named-section *elf* ".text")))))))
           (let ((addr (address (sh .text))))
             (mapcar (lambda (_)
                       (declare (ignorable _))
                       (prog1 addr (incf addr)))
                     (coerce (data .text) 'list))))))))

;;; elf-test.lisp ends here
