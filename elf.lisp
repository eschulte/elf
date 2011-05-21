;;; elf.lisp --- A Common Lisp library for manipulating ELF files

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
;; required so that :com.gigamonkeys.binary-data can change :common-lisp stuff
#+clisp
(when (ext:package-lock :common-lisp) (setf (ext:package-lock :common-lisp) nil))
#+sbcl
(unlock-package :common-lisp)
(in-package #:elf)


;;; Basic Binary types
;; strings
(define-binary-type string (length)
  (:reader (in)
           (let ((string (make-string length)))
             (dotimes (i length)
               (setf (char string i) (code-char (read-byte in))))
             string))
  (:writer (out string)
           (dotimes (i length)
             (write-byte (char-code (char string i)) out))))

(define-binary-type terminated-string ()
  (:reader (in)
           (with-output-to-string (s)
             (loop for char = (code-char (read-byte in))
                until (char= char +null+) do (write-char char s))))
  (:writer (out string)
           (loop for char across string
              do (write-byte (char-code char) out)
              finally (write-byte (char-code +null+) out))))

;; raw bytes
(define-binary-type raw-bytes (length)
  (:reader (in)
           (let ((buf (make-array length :element-type '(unsigned-byte 8))))
             (read-sequence buf in)
             buf))
  (:writer (out buf)
           (write-sequence buf out)))

;; dictionaries
(defmacro define-elf-dictionary
    (name num dictionary &key (signed nil) (class-dependent nil))
  (let ((byte-form `(if ,class-dependent
                        (case *class*
                          (:32-bit ,num)
                          (:64-bit ,(* 2 num))
                          (t (error 'bad-elf-class :class *class*)))
                        ,num)))
    `(define-binary-type ,name ()
       (:reader (in)
                (let ((byte (bytes-from in ,byte-form ,signed)))
                  (or (cdr (assoc byte ',dictionary)) byte)))
       (:writer (out val)
                (bytes-to out ,byte-form
                          (if (numberp val)
                              val
                              (car (rassoc val ',dictionary))) ,signed)))))

;; integers and bytes
(defvar *endian* 'little
  "Controls the endianness of how bytes are read.")

(defvar *class* nil
  "Word size of the machine, (e.g. :32-bit or :64-bit).")

(define-condition bad-elf-class (error)
  ((class :initarg :class :reader class)))

(defun bytes-to-int (bytes &optional signed-p &aux steps)
  (dotimes (n (length bytes)) (setf steps (cons (* n 8) steps)))
  (unless (listp bytes) (setf bytes (coerce bytes 'list)))
  (let ((value 0))
    (mapc (lambda (bit b) (setf (ldb (byte 8 bit) value) b))
          (if (eq *endian* 'little) (reverse steps) steps) bytes)
    (if (and signed-p (> value (expt 2 (1- (* 8 (length bytes))))))
        (- (expt 2 (1- (* 8 (length bytes)))) value)
        value)))

(defun int-to-bytes (int size &optional signed-p &aux steps)
  (dotimes (n size) (setf steps (cons (* n 8) steps)))
  (let ((buf (make-array size
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0)))
    (when (and signed-p (< int 0))
      (setf int (+ (expt 2 (1- (* 8 size))) (- 0 int))))
    (mapc (lambda (bit) (vector-push (ldb (byte 8 bit) int) buf))
          (if (eq *endian* 'little) (reverse steps) steps))
    buf))

(defun bytes-from (in bytes &optional signed-p)
  (let ((buf (make-array bytes :element-type '(unsigned-byte 8))))
    (read-sequence buf in)
    (bytes-to-int (coerce buf 'list) signed-p)))

(defun bytes-to (out bytes value &optional signed-p)
  (write-sequence (int-to-bytes value bytes signed-p) out))

(define-binary-type unsigned-integer (bytes)
  (:reader (in) (bytes-from in bytes))
  (:writer (out value) (bytes-to out bytes value)))

(define-binary-type signed-integer (bytes)
  (:reader (in) (bytes-from in bytes 'signed))
  (:writer (out value) (bytes-to out bytes value 'signed)))

(define-binary-type class-dependent-unsigned-integer (bytes)
  (:reader (in)
           (case *class*
             (:32-bit (bytes-from in bytes))
             (:64-bit (bytes-from in (* 2 bytes)))
             (t (error 'bad-elf-class :class *class*))))
  (:writer (out value)
           (case *class*
             (:32-bit (bytes-to out bytes value))
             (:64-bit (bytes-to out (* 2 bytes) value))
             (otherwise (error 'bad-elf-class :class *class*)))))

(define-binary-type class-dependent-signed-integer (bytes)
  (:reader (in)
           (case *class*
             (:32-bit (bytes-from in bytes 'signed))
             (:64-bit (bytes-from in (* 2 bytes) 'signed))
             (t (error 'bad-elf-class :class *class*))))
  (:writer (out value)
           (case *class*
             (:32-bit (bytes-to out bytes value 'signed))
             (:64-bit (bytes-to out (* 2 bytes) value 'signed))
             (otherwise (error 'bad-elf-class :class *class*)))))

(define-binary-type char   () (unsigned-integer :bytes 1))
(define-binary-type half   () (unsigned-integer :bytes 2))
(define-binary-type word   () (unsigned-integer :bytes 4))
(define-binary-type sword  () (signed-integer   :bytes 4))
(define-binary-type addr   () (class-dependent-unsigned-integer :bytes 4))
(define-binary-type off    () (class-dependent-unsigned-integer :bytes 4))
(define-binary-type xword  () (class-dependent-unsigned-integer :bytes 4))
(define-binary-type sxword () (class-dependent-signed-integer   :bytes 4))

;; ELF dictionaries -- ELF header
(define-elf-dictionary elf-type 2
  ((0 . :none)
   (1 . :relocatable)
   (2 . :executable)
   (3 . :shared-object)
   (4 . :core)))

(define-elf-dictionary elf-machine 2
  ((0 . :no-machine)
   (1 . :at&t-we-32100)
   (2 . :sun-sparc)
   (3 . :intel-80386)
   (4 . :motorola-m68k-family)
   (5 . :motorola-m88k-family)
   (7 . :intel-80860)
   (8 . :mips-r3000-big-endian)))

(define-elf-dictionary elf-version 4
  ((0 . :invalid) (1 . :current)))

(let ((dictionary '((0 . :invalid) (1 . :32-bit) (2 . :64-bit))))
  ;; this special dictionary will set the value of the *class* special
  ;; variable as it reads the file class from the elf header
  (define-binary-type elf-class ()
    (:reader (in)
             (let ((byte (bytes-from in 1)))
               (setf *class* (or (cdr (assoc byte dictionary)) byte))))
    (:writer (out val)
             (let ((value (if (numberp val) val (car (rassoc val dictionary)))))
               (bytes-to out 1 value)))))

;; section header table
(define-elf-dictionary sh-type 4
  ((0 . :null)
   (1 . :progbits)
   (2 . :symtab)
   (3 . :strtab)
   (4 . :rela)
   (5 . :hash)
   (6 . :dynamic)
   (7 . :note)
   (8 . :nobits)
   (9 . :rel)
   (10 . :shlib)
   (11 . :dynsym)))

(define-elf-dictionary sh-flags 4
  ((1 . :writable)
   (2 . :allocatable)
   (4 . :executable))
  :class-dependent t)

;; program header table
(define-elf-dictionary ph-type 4
  ((0 . :null)
   (1 . :load)
   (2 . :dynamic)
   (3 . :interp)
   (4 . :note)
   (5 . :shlib)
   (6 . :phdr)
   (1685382480 . :gnu_eh_frame)
   (1685382481 . :gnu_stack)
   (1685382482 . :gnu_relro)
   (1879048186 . :sunwbss)
   (1879048187 . :sunwstack)))

;; .dynamic section tag
(define-elf-dictionary dyn-tag 4
  ((0  . :null)
   (1  . :needed)
   (2  . :pltrelsz)
   (3  . :pltgot)
   (4  . :hash)
   (5  . :strtab)
   (6  . :symtab)
   (7  . :rela)
   (8  . :relasz)
   (9  . :relaent)
   (10 . :strsz)
   (11 . :syment)
   (12 . :init)
   (13 . :fini)
   (14 . :soname)
   (15 . :rpath)
   (16 . :symbolic)
   (17 . :rel)
   (18 . :relsz)
   (19 . :relent)
   (20 . :pltrel)
   (21 . :debug)
   (22 . :textrel)
   (23 . :jmprel))
  :signed t :class-dependent t)


;;; Classes, readers and writers
(define-binary-class elf-header ()
  (;; elf ident
   (magic-number  (string :length    4))
   (file-class    elf-class)
   (data-encoding char)
   (ident-version char)
   (padding       (raw-bytes :length 8))
   (ei-size       char)
   ;; elf header
   (type          elf-type)
   (machine       elf-machine)
   (version       elf-version)
   (entry         addr)
   (phoff         off)
   (shoff         off)
   (flags         word)
   (eh-size       half)
   (ph-ent-size   half)
   (ph-num        half)
   (sh-ent-size   half)
   (sh-num        half)
   (sh-str-ind    half)))

(defun elf-header-size ()
  (case *class*
    (:32-bit 52)
    (:64-bit 64)
    (otherwise (error 'bad-elf-class :class *class*))))

(define-binary-class section-header ()
  ((name      word)
   (type      sh-type)
   (flags     sh-flags)
   (address   addr)
   (offset    off)
   (size      xword)
   (link      word)
   (info      word)
   (addralign xword)
   (entsize   xword)))

(define-binary-class program-header-32 ()
  ((type   ph-type)
   (offset off)
   (vaddr  addr)
   (paddr  addr)
   (filesz word)
   (memsz  word)
   (flags  word)
   (align  word)))

(define-binary-class program-header-64 ()
  ((type   ph-type)
   (flags  word)
   (offset off)
   (vaddr  addr)
   (paddr  addr)
   (filesz xword)
   (memsz  xword)
   (align  xword)))

(defun program-header-type ()
  "Return the appropriate type of program header given the value of *CLASS*."
  (case *class*
    (:32-bit 'program-header-32)
    (:64-bit 'program-header-64)
    (otherwise (error 'bad-elf-class :class *class*))))

(defclass elf-rel () ())

(define-binary-class elf-rel-32 (elf-rel)
  ((offset addr)
   (info   word)))

(define-binary-class elf-rel-64 (elf-rel)
  ((offset addr)
   (info   xword)))

(defun elf-rel-type ()
  "Return the appropriate type of elf relocation given the value of *CLASS*."
  (case *class*
    (:32-bit 'elf-rel-32)
    (:64-bit 'elf-rel-64)
    (otherwise (error 'bad-elf-class :class *class*))))

(defmethod rel-sym ((rel elf-rel))
  (ash (info rel)
       (case (class-name (class-of rel))
         ((elf-rel-32 elf-rela-32) -8)
         ((elf-rel-64 elf-rela-64) -32))))

(defvar rel-types
  '((:intel-80386
     (0  . :none)
     (1  . :32)
     (2  . :pc32)
     (3  . :got32)
     (4  . :plt32)
     (5  . :copy)
     (6  . :glob-dat)
     (7  . :jmp-slot)
     (8  . :relative)
     (9  . :gotoff)
     (10 . :gotpc)))
  "Association list of type meaning by machine type.")

(defmethod rel-type ((rel elf-rel) (header elf-header))
  "The interpretation of the type is machine specific."
  (let ((val (logand (info rel)
                     (case (class-name (class-of rel))
                       ((elf-rel-32 elf-rela-32) #xff)
                       ((elf-rel-64 elf-rela-64) #xffffffff)))))
    (flet ((get (item lst) (cdr (assoc item lst))))
      (or (get val (get (machine header) rel-types)) val))))

(defun rel-info (sym type)
  "Convert a symbol and type back into the info field of an elf-rel."
  (case *class*
    (:32-bit (+ (ash sym 8) (logand type #xff)))
    (:64-bit (+ (ash sym 32) type))
    (otherwise (error 'bad-elf-class :class *class*))))

(define-binary-class elf-rela-32 (elf-rel)
  ((offset addr)
   (info   word)
   (addend sword)))

(define-binary-class elf-rela-64 (elf-rel)
  ((offset addr)
   (info   xword)
   (addend sxword)))

(defun elf-rela-type ()
  "Return type of elf relocation (w/addend) given the value of *CLASS*."
  (case *class*
    (:32-bit 'elf-rela-32)
    (:64-bit 'elf-rela-64)
    (otherwise (error 'bad-elf-class :class *class*))))

(defclass elf-sym ()
  ((sym-name :initform nil :accessor sym-name)))

(define-binary-class elf-sym-32 (elf-sym)
  ((name  word)
   (value addr)
   (size  word)
   (info  char)
   (other char)
   (shndx half)))

(define-binary-class elf-sym-64 (elf-sym)
  ((name  word)
   (info  char)
   (other char)
   (shndx half)
   (value addr)
   (size  xword)))

(defun elf-sym-type ()
  "Return the appropriate type of elf symbol given the value of *CLASS*."
  (case *class*
    (:32-bit 'elf-sym-32)
    (:64-bit 'elf-sym-64)
    (otherwise (error 'bad-elf-class :class *class*))))

(defclass elf-dyn () ())

(define-binary-class elf-dyn-32 (elf-dyn)
  ((tag dyn-tag)
   (un (raw-bytes :length 4))))

(define-binary-class elf-dyn-64 (elf-dyn)
  ((tag dyn-tag)
   (un (raw-bytes :length 8))))

(defun elf-dyn-type ()
  "Return the appropriate type of dynamic symbol given the value of *CLASS*."
  (case *class*
    (:32-bit 'elf-dyn-32)
    (:64-bit 'elf-dyn-64)
    (otherwise (error 'bad-elf-class :class *class*))))

(defmethod word-size ((dyn elf-dyn-32)) 4)
(defmethod word-size ((dyn elf-dyn-64)) 8)

(defmethod un-type ((dyn elf-dyn))
  (let ((val (list :pltrel :relent :relsz :rpath :soname :syment
                   :strsz :relaent :relasz :pltrelsz :needed))
        (ptr (list :pltgot :hash :strtab :symtab :rela :init
                   :fini :rel :debug :jmprel))
        (ignored (list :null :symbolic :textrel)))
    (cond ((member (tag dyn) val)     :val)
          ((member (tag dyn) ptr)     :ptr)
          ((member (tag dyn) ignored) :ignored))))

(defmethod ptr ((dyn elf-dyn))
  (when (equal (un-type dyn) :ptr) (bytes-to-int (un dyn))))

(defmethod (setf ptr) (new (dyn elf-dyn))
  (if (equal :ptr (un-type dyn))
      (setf (un dyn) (coerce (int-to-bytes new (word-size dyn)) 'vector))
      (error "Can't set ptr for dynamic section of type ~a" (tag dyn))))

(defmethod val ((dyn elf-dyn))
  (when (equal (un-type dyn) :val) (bytes-to-int (un dyn) 'signed)))

(defmethod (setf val) (new (dyn elf-dyn))
  (if (equal :val (un-type dyn))
      (setf (un dyn) (coerce (int-to-bytes
                              new (word-size dyn) 'signed) 'vector))
      (error "Can't set val for dynamic section of type ~a" (tag dyn))))

(defun dynamic-entry (sec)
  "Return the entry in .dynamic associated with SEC."
  (let* ((tags (dolist (dyn (data (named-section (elf sec) ".dynamic")))
                 (when (keywordp (tag dyn))
                   (cons (symbol-name (tag dyn)) dyn))))
         (dyn (cdr (assoc (string-upcase (subseq (name sec) 1))
                          tags :test #'string=))))
    (when (and dyn (equal (vma sec) (ptr dyn))) dyn)))

(defmethod binding ((sym elf-sym))
  (let ((val (ash (info sym) -4)))
    (case val
      (0 :local)
      (1 :global)
      (2 :weak)
      (t val))))

(defmethod type ((sym elf-sym))
  (let ((val (logand (info sym) #xf)))
    (case val
      (0 :notype)
      (1 :object)
      (2 :func)
      (3 :section)
      (4 :file)
      (t val))))

(defclass section ()
  ((elf  :initarg :elf  :accessor elf)
   (sh   :initarg :sh   :accessor sh)
   (ph   :initarg :ph   :accessor ph)
   (name :initarg :name :accessor name)
   (data :initarg :data :reader data :writer set-data)))

(defmethod offset ((sec section))
  (offset (sh sec)))

(defmethod (setf offset) (new (sec section))
  (setf (offset (sh sec)) new)
  (when (and (ph sec) (= (offset sec) (offset (ph sec))))
    (setf (offset (ph sec)) new)))

(defmethod vma ((sec section))
  (when (ph sec) (+ (vaddr (ph sec)) (- (offset sec) (offset (ph sec))))))

(defmethod (setf vma) (new (sec section))
  (declare (ignorable new))
  (error "Don't set the VMA, this is calculated from the ph and offset."))

(defmethod size ((sec section))
  (size (sh sec)))

(defmethod (setf size) (new (sec section))
  (let ((delta (- new (size sec))))
    (setf (size (sh sec)) new)
    (when (ph sec)
      ;; TODO: probably need a better test here at some point
      (when (= (filesz (ph sec)) (memsz (ph sec)))
        (setf (memsz (ph sec)) (+ delta (memsz (ph sec)))))
      (setf (filesz (ph sec)) (+ delta (filesz (ph sec)))))))

(defmethod type ((sec section))
  (type (sh sec)))

(defmethod (setf type) (new (sec section))
  (setf (type (sh sec)) new))

(defmethod flags ((sec section))
  (flags (concatenate 'list (ph sec) (sh sec))))

(defmethod (setf flags) (new (sec section))
  (declare (ignorable new))
  (error "TODO: setting flags not yet implemented"))

(defmethod alignment ((sec section))
  (if (ph sec) (align (ph sec)) (addralign (sh sec))))

(defmethod (setf alignment) (new (sec section))
  (declare (ignorable new))
  (error "TODO: setting alignment is not yet implemented"))

(defclass elf ()
  ((header        :initarg :header        :accessor header)
   (section-table :initarg :section-table :accessor section-table)
   (program-table :initarg :program-table :accessor program-table)
   (sections      :initarg :sections      :accessor sections)
   (ordering      :initarg :ordering      :accessor ordering)))

(defun program-header-for-section (pt sec)
  "Return the program header in PT associated with SEC.
Note: a section may be included in multiple program headers, but for
now lets just select the one with the tightest bounds on the
section (in the file)."
  (first (sort (remove-if (complement (lambda (ph)
                                        (and (>= (offset sec) (offset ph))
                                             (< (+ (offset sec) (size sec))
                                                (+ (offset ph) (filesz ph))))))
                          pt) (lambda (a b) (> (filesz a) (filesz b))))))

(defmethod read-value ((type (eql 'elf)) in &key)
  "Read an elf object from a binary input stream."
  (let ((e (make-instance 'elf)))
    (with-slots (header section-table program-table sections ordering) e
      (setf
       header        (read-value 'elf-header in)
       section-table (unless (zerop (shoff header))
                       (progn (file-position in (shoff header))
                              (loop for x from 0 to (1- (sh-num header))
                                 collect (read-value 'section-header in))))

       program-table (unless (zerop (phoff header))
                       (progn (file-position in (phoff header))
                              (loop for x from 0 to (1- (ph-num header))
                                 collect
                                   (read-value (program-header-type) in))))
       sections
       (let ((str-off (offset (nth (sh-str-ind header) section-table))))
         (mapcar
          (lambda (h)
            (let ((section (make-instance 'section)))
              (with-slots (elf sh ph name data) section
                (setf elf e
                      sh h
                      ph (program-header-for-section program-table section)
                      name (progn (file-position in (+ str-off (name h)))
                                  (read-value 'terminated-string in))
                      data ;; data can vary based on the specific section
                      (cond
                        ((or (string= ".dynsym" name) (string= ".symtab" name))
                         (file-position in (offset sh))
                         (loop for x from 0 to (1- (/ (size sh) (entsize sh)))
                            collect (read-value (elf-sym-type) in)))
                        ((string= ".dynamic" name)
                         (file-position in (offset sh))
                         (loop for x from 0 to (1- (/ (size sh) (entsize sh)))
                            collect (read-value (elf-dyn-type) in)))
                        ((equal :rel (type sh))
                         (file-position in (offset sh))
                         (loop for x from 0 to (1- (/ (size sh) (entsize sh)))
                            collect (read-value (elf-rel-type) in)))
                        ((equal :rela (type sh))
                         (file-position in (offset sh))
                         (loop for x from 0 to (1- (/ (size sh) (entsize sh)))
                            collect (read-value (elf-rela-type) in)))
                        (t
                         (file-position in (offset h))
                         (read-value 'raw-bytes in :length (size section))))))
              section))
          section-table))
       ordering
       (let ((parts (remove-if
                     (lambda (group) (zerop (car group)))
                     (mapcar (lambda (sec) (list (offset sec) (size sec) (name sec)))
                             sections))))
         (flet ((read-at (beg size)
                  (progn (file-position in beg)
                         (read-value 'raw-bytes in :length size)))
                (add-part (offset size contents)
                  (setf parts (cons (list offset size contents) parts))))
           (add-part 0 (elf-header-size) :header)
           (add-part (shoff header)     ; add section-table
                     (* (sh-num header) (sh-ent-size header)) :section-table)
           (add-part (phoff header)     ; add program-table
                     (* (ph-num header) (ph-ent-size header)) :program-table)
           (add-part (file-length in) 0 :end)
           (setf parts (sort parts (lambda (a b) (< (first a) (first b)))))
           ;; return ordered list of items with filler
           (butlast ;; pop off the end marker
            (mapcar ;; just return the identifier
             (lambda (el) (car (last el)))
             (nreverse
              (reduce
               (lambda (acc part)
                 (bind ((((offset size name) &rest rest) acc))
                   (declare (ignorable name rest))
                   (let* ((end (+ offset size))
                          (size (- (first part) end)))
                     (if (> size 0) ; is there a gap insert those bytes
                         (append (list part (list end size (read-at end size)))
                                 acc)
                         (cons part acc)))))
               (cdr parts) :initial-value (list (car parts))))))))))
    ;; initialize symbol names in .symtab
    (let ((tab (coerce (data (named-section e ".strtab")) 'list)))
      (mapcar (lambda (sym)
                (setf (sym-name sym)
                      (coerce (loop for code in (subseq tab (name sym))
                                 until (equal code 0)
                                 collect (code-char code))
                              'string)))
              (data (named-section e ".symtab"))))
    e))

(defmethod write-value ((type (eql 'elf)) out value &key)
  "Write an elf object to a binary output stream."
  (with-slots (header section-table program-table sections ordering) value
    (flet ((write-bytes (bytes)
             (dolist (b (coerce bytes 'list)) (write-byte b out))))
      (dolist (chunk ordering)
        (cond
          ((stringp chunk)              ; named section
           (let ((sec (named-section value chunk)))
             (unless (equal :nobits (type sec))
               (cond
                 ((or (string= ".dynsym" (name sec))
                      (string= ".symtab" (name sec)))
                  (dolist (sym (data sec))
                    (write-value (elf-sym-type) out sym)))
                 ((string= ".dynamic" (name sec))
                  (dolist (dyn (data sec))
                    (write-value (elf-dyn-type) out dyn)))
                 ((equal :rel (type sec))
                  (dolist (rel (data sec))
                    (write-value (elf-rel-type) out rel)))
                 ((equal :rela (type sec))
                  (dolist (rel (data sec))
                    (write-value (elf-rela-type) out rel)))
                 (t (write-bytes (data sec)))))))
          ((vectorp chunk)              ; raw filler
           (write-bytes chunk))
          ((eq :header chunk)           ; header
           (write-value 'elf-header out header))
          ((eq :section-table chunk)    ; section table
           (mapc (lambda (c) (write-value 'section-header out c)) section-table))
          ((eq :program-table chunk)    ; program table
           (mapc (lambda (c)
                   (write-value (program-header-type) out c))
                 program-table)))))))

(defun copy-elf (elf)
  (unless (eql 'elf (class-name (class-of elf)))
    (error "~&`copy-elf' called on non-elf object: ~a" elf))
  (let ((e (make-instance 'elf)))
    (with-slots (header section-table program-table sections ordering) e
      (setf
       header (generic-copy (header elf))
       section-table (generic-copy (section-table elf))
       program-table (generic-copy (program-table elf))
       sections
       (let ((ish (indexed (section-table elf)))
             (iph (indexed (program-table elf))))
         (flet ((copy-between-tables (hd i-tab to-tab)
                  (when hd
                    (let ((ind (caar (member-if (lambda (ih) (equal hd (cadr ih)))
                                                i-tab))))
                      (unless ind (error "~&header:~a not in table:~a" hd i-tab))
                      (nth ind to-tab)))))
           (mapcar
            (lambda (sec)
              (let ((s (make-instance 'section)))
                (with-slots (elf sh ph name data) s
                  (setf elf e
                        sh (copy-between-tables (sh sec) ish section-table)
                        ph (copy-between-tables (ph sec) iph program-table)
                        name (name sec)
                        data (copy-seq (data sec)))
                  s)))
            (sections elf))))
       ordering (generic-copy (ordering elf))))
    e))

(defmethod (setf data) (new (sec section))
  "Update the contents of section to new, and update all headers appropriately."
  ;; step through the ordered sections, updating where required
  (with-slots (elf sh name data) sec
    (let* ((old-length (* (length data) (if (vectorp data) 1 (entsize sh))))
           (new-length (* (length new)  (if (vectorp new)  1 (entsize sh))))
           (end-point  (+ (offset sec) old-length))
           (delta      (- new-length old-length))
           ;; a delta to decremented as sections change sizes
           (sec-deltas (list (cons end-point delta)))
           last)
      (unless (= old-length new-length)
        (setf
         (ordering elf)
         (loop for chunk in (ordering elf)
            if (equal chunk name)
            do (setf last (+ (offset sec) new-length))
            else do
              (let ((d (cdar sec-deltas)))
                (when (and last (> d 0))
                  (cond
                    ((stringp chunk)      ; section
                     (let ((sec (named-section elf chunk)))
                       (when (not (zerop (address sh)))
                         (when-let ((dyn (dynamic-entry sec)))
                           (setf (ptr dyn) (+ (address sh) d)))
                         (setf (address sh) (+ (address sh) d)))
                       (setf (offset sec) last)
                       (setf last (+ (offset sec) (size sec)))))
                    ((vectorp chunk)      ; padding bytes
                     (if (> d (length chunk))
                         (progn (setq d (- d (length chunk)))
                                (setq chunk nil))
                         (progn (setq last (+ last (- (length chunk) d)))
                                (setq chunk (subseq chunk d))
                                (setq d 0))))
                    ((equal :program-table chunk) ; program header
                     (let ((header (header elf)))
                       (setf (phoff header) (+ (phoff header) d))
                       (setf last (+ (phoff header)
                                     (* (ph-num header) (ph-ent-size header))))))
                    ((equal :section-table chunk) ; section header
                     (let ((header (header elf)))
                       (setf (shoff header) (+ (shoff header) d))
                       (setf last (+ (shoff header)
                                     (* (sh-num header)
                                        (sh-ent-size header))))))))
                (setf sec-deltas (cons (cons last d) sec-deltas)))
            when chunk collect chunk)))
      ;; sec-deltas should be in increasing order by offset w/o changed section
      (setq sec-deltas (nreverse (butlast sec-deltas)))
      ;; update the dynamic symbols used at run time
      (let ((ds (deltas data new)))
        (dolist (sym (data (named-section elf ".dynsym")))
          (with-slots (value) sym
            (when (> value (offset sec))
              (setf value
                    (+ value
                       (if (and (>= value (offset sec))
                                (<= value (+ (offset sec) (size sec))))
                           ;; inside of the changed section
                           (aref ds (- value (offset sec)))
                           ;; after the changed section
                           (or (cdr (assoc-if (lambda (p) (> value p)) sec-deltas))
                               0))))))))
      ;; update size and the contents of the section
      (setf (size sec) new-length)
      (set-data new sec))))


;;; Misc functions
(defun named-section (elf name)
  "Return the section in ELF named NAME."
  (first (remove-if (lambda (sec) (not (string= name (name sec)))) (sections elf))))

(defmethod symbols ((elf elf))
  "Return the symbols contained in ELF."
  (data (named-section elf ".symtab")))

(defun named-symbol (elf name)
  "Return the symbol in ELF named NAME."
  (first (remove-if (lambda (sec) (not (string= name (sym-name sec))))
                    (symbols elf))))

(defun elf-p (file)
  "Return t if file is an ELF file (using the magic number test)."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (string= (concatenate 'string (string (code-char #x7f)) "ELF")
             (read-value 'string in :length 4))))

(defun read-elf (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'elf in)))

(defun write-elf (elf file)
  (with-open-file (out file :direction :output :element-type '(unsigned-byte 8))
    (write-value 'elf out elf))
  nil)

(defun show-dynamic (elf)
  "Show the dynamic symbols of ELF in a manner similar to readelf."
  (let ((dynamic (named-section elf ".dynamic")))
    (format t "~&Dynamic section at offset 0x~x contains ~d entries:~%"
            (offset dynamic) (length (data dynamic)))
    (format t " Tag Name/Value~%")
    (loop for dyn in (data dynamic)
       do (format t " ~10a ~a~%" (tag dyn) (or (val dyn) (ptr dyn)))
       until (equal :null (tag dyn)))))

(defun show-symbols (elf)
  "Show all symbols in ELF in a manner similar to readelf."
  (flet ((string-at (data offset)
           (coerce (loop for ch in (subseq data offset)
                        until (= (char-code ch) 0)
                      collect ch)
                   'string)))
    (let ((dynsym (named-section elf ".dynsym"))
          (dynstr (mapcar #'code-char
                          (coerce (data (named-section elf ".dynstr")) 'list)))
          (symtab (named-section elf ".symtab"))
          (strtab (mapcar #'code-char
                          (coerce (data (named-section elf ".strtab")) 'list))))
      (dolist (tab (list dynsym symtab))
        (format t "~&~%Symbol table '~a' contains ~d entries:~%"
                (name tab) (length (data tab)))
        (format t "   Num:    Value  Size Type     Bind  Name~%")
        (loop for sym in (data tab) as i from 0
           do (format t "~6d: ~8x ~5d ~8a ~6a ~a~%"
                      i (value sym) (size sym) (type sym) (binding sym)
                      (string-at
                       (if (string= ".dynstr" (name tab)) dynstr strtab)
                       (name sym))))))))

(defun show-file-layout (elf)
  "Show the layout of the elements of an elf file with binary offset."
  (let ((beg 0))
    (format
     t "~:{~&~8a ~8a ~18a ~8a~}~%"
     (cons
      (list 'start 'offset 'contents 'end)
      (mapcar
       (lambda (el)
         (let ((size (cond
                       ((stringp el)
                        (let ((sec (named-section elf el)))
                          (cond
                            ((equal :nobits (type sec)) 0)
                            ((member (name sec) '(".dynsym" ".dynamic" ".symtab")
                                     :test #'string=)
                             (* (length (data sec)) (entsize (sh sec))))
                            (t (length (data sec))))))
                       ((vectorp el) (length el))
                       ((eq :header el) (elf-header-size))
                       ((eq :section-table el)
                        (* (sh-ent-size (header elf)) (sh-num (header elf))))
                       ((eq :program-table el)
                        (* (ph-ent-size (header elf)) (ph-num (header elf))))
                       (t (error "~&unknown ordering element:~a" el)))))
           (list beg
                 (if (stringp el)
                     (offset (sh (named-section elf el)))
                     :none)
                 (cond ((stringp el) el) ((vectorp el) :filler) (t el))
                 (setf beg (+ beg size)))))
       (ordering elf))))))

(defun show-memory-layout (elf)
  "Show the layout of the elements of an elf file with binary offset."
  (format t "~&addr     contents          end     ~%")
  (format t "-------------------------------------~%")
  (last
   (with-slots (sections section-table program-table) elf
     (mapc
      (lambda (trio)
        (bind (((beg size name) trio))
          (format t "~&0x~x ~18a 0x~x" beg name (+ beg size))))
      (stable-sort
       (remove-if
        (lambda (trio) (zerop (first trio)))
        (append
         (mapcar (lambda (head)
                   (list (vaddr head) (memsz head) (type head)))
                 program-table)
         (mapcar (lambda (sec)
                   (list (address (sh sec)) (size sec) (name sec)))
                 sections))) #'< :key #'car)))))


;;; disassembly functions using objdump from GNU binutils
(defmethod objdump ((sec section))
  "Use objdump to return the disassembly of SEC."
  (let ((path (temp-file-name)))
    (write-elf (elf sec) path)
    (unwind-protect
         (shell-command (format nil "objdump -j ~a -d ~a" (name sec) path))
      (delete-file path))))

(defun parse-addresses (lines)
  "Parse addresses from lines of objdump output."
  (mapcar
   (lambda (line)
     (list
      ;; address in memory
      (parse-integer (subseq line 1 8) :radix 16)
      ;; bytes
      (mapcar (lambda (num) (parse-integer num :radix 16))
              (split-sequence
               #\Space
               (trim  (subseq line 10 (if (> (length line) 32) 31 nil)))))
      ;; disassembled assembly text
      (when (> (length line) 31) (trim (subseq line 32)))))
   (remove-if (lambda (line)
                (or (< (length line) 9)
                    (not (equal #\: (aref line 8)))))
              lines)))

(defun objdump-parse (output)
  "Parse the output of `objdump' returning the disassembly by symbol."
  (let ((lines (split-sequence #\Newline output))
        (sec-header (lambda-registers (addr name) "^([0-9a-f]+) <(.+)>:$"
                      (cons (parse-integer addr :radix 16) name))))
    (mapcar #'list
            (remove nil (mapcar sec-header lines))
            (mapcar #'parse-addresses
                    (cdr (split-sequence-if sec-header lines))))))

;;; elf.lisp ends here
