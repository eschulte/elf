;;; elf-test.lisp --- Tests for elf.lisp

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :elf-test)

(defsuite test)
(in-suite test)


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
  (let ((*test-class* :32-bit)) (test))
  (let ((*test-class* :64-bit)) (test)))

(defun system-class ()
  (ecase (parse-integer (elf::shell "getconf LONG_BIT"))
    (32 :32-bit)
    (64 :64-bit)))


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
                  (member (elf:type sec) '(:rel :rela)))
        (is (= (size (sh sec)) (length (data sec)))
            "section:~a data:~a != size:~a"
            (name sec) (length (data sec)) (size (sh sec)))))))

(deftest test-idempotent-read-write ()
  (with-fixture hello-elf
    (is (equal *test-class* *class*))
    (test-write-elf *elf* *tmp-file*)
    (is (elf::equal-it *elf* (test-read-elf *tmp-file*)))))

(deftest test-write-working-executable ()
  (with-fixture hello-elf
    (test-write-elf *elf* *tmp-file*)
    (is (probe-file *tmp-file*))
    #-ccl
    (when (equal (system-class) *test-class*)
      (elf::shell (format nil "chmod +x ~a" *tmp-file*))
      (is (equal "hello world" (elf::trim (elf::shell *tmp-file*)))))))

(deftest test-tweaked-text-working-executable ()
  (with-fixture hello-elf
    ;; change a `noop' which is not on the execution path to a `ret'
    (setf (aref (data (named-section *elf* ".text")) 42) #xc3)
    (test-write-elf *elf* *tmp-file*)
    (is (probe-file *tmp-file*))
    #-ccl
    (when (equal (system-class) *test-class*)
      (elf::shell (format nil "chmod +x ~a" *tmp-file*))
      (is (equal "hello world" (elf::trim (elf::shell *tmp-file*)))))))

(deftest test-data-setf-changes ()
  (with-fixture hello-elf
    (let ((first-data (copy-seq (data (named-section *elf* ".text")))))
      (setf (aref (data (named-section *elf* ".text")) 42) #xc3)
      (is (not (elf::equal-it first-data
                              (data (named-section *elf* ".text"))))))))

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
    (is (elf::equal-it '(0) (second (nth 12 (parse-addresses lines)))))))

(deftest test-equality-of-data-and-objdump-bytes ()
  (with-fixture hello-elf
    (let ((.text (named-section *elf* ".text")))
      (is (elf::equal-it (coerce (data .text) 'list)
                    (apply #'append
                           (apply #'append
                                  (mapcar
                                   (lambda (sym) (mapcar #'second (cdr sym)))
                                   (objdump-parse (objdump .text))))))))))

(deftest test-addresses-compared-w/objdump-output ()
  (with-fixture hello-elf
    (let ((.text (named-section *elf* ".text")))
      (is (elf::equal-it
           (apply #'append
                  (mapcar
                   (lambda-bind ((addr bytes disasm))
                     (declare (ignorable disasm))
                     (mapcar (lambda (_)
                               (declare (ignorable _))
                               (prog1 addr (incf addr))) bytes))
                   (apply #'append
                          (mapcar #'cdr
                                  (objdump-parse
                                   (objdump (named-section *elf* ".text")))))))
           (let ((addr (address (sh .text))))
             (mapcar (lambda (_)
                       (declare (ignorable _))
                       (prog1 addr (incf addr)))
                     (coerce (data .text) 'list))))))))

;;; elf-test.lisp ends here
