;;; util.lisp --- utility functions for elf.lisp

;; For information on the elf format see the following
;; http://www.muppetlabs.com/~breadbox/software/ELF.txt

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :elf)

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
  #+allegro
  (system:make-temp-file-name)
  #-(or sbcl clisp ccl allegro)
  (error "no temporary file backend for this lisp."))

(defmacro with-temp-file (file &rest body)
  "SPEC is the variable used to reference the file w/optional extension.
After BODY is executed the temporary file is removed."
  `(let ((,file (temp-file-name)))
     (unwind-protect (progn ,@body)
       (when (probe-file ,file) (delete-file ,file)))))

(defun trim (str &key (chars '(#\Space #\Tab #\Newline)))
  (loop until (or (emptyp str) (not (member (aref str 0) chars)))
     do (setf str (subseq str 1)))
  (loop until (or (emptyp str) (not (member (aref str (1- (length str))) chars)))
     do (setf str (subseq str 0 (1- (length str)))))
  str)

(defun shell (command)
  #+ecl          (ext:system command)
  #+ccl          (shell-command command :input "")
  #-(or ccl ecl) (shell-command command))

;;; generic forensic functions over arbitrary objects
(defun my-slot-definition-name (el)
  #+sbcl
  (sb-mop::slot-definition-name el)
  #+ccl
  (ccl:slot-definition-name el)
  #+ecl
  (error "ECL does not support `my-slot-definition-name'")
  #-(or sbcl ccl ecl)
  (clos::slot-definition-name el))

(defun my-class-slots (el)
  #+sbcl
  (sb-mop::class-slots el)
  #+ccl
  (ccl:class-slots el)
  #+ecl
  (error "ECL does not support `my-class-slots'")
  #-(or sbcl ccl ecl)
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

(defun show-it (hd &key (out t))
  "Print the fields of a elf, section or program header.
Optional argument OUT specifies an output stream."
  (format (or out t) "~&")
  (mapcar
   (lambda (slot)
     (let ((val (slot-value hd slot)))
       (format out "~s:~a " slot val)
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
