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
(in-package #:cl-user)
;; uses quicklisp for dependencies -- http://www.quicklisp.org/ 
(ql:quickload :com.gigamonkeys.binary-data)
(ql:quickload :cl-ppcre)
(ql:quickload :metabang-bind)
(defpackage #:elf
  (:use :common-lisp :com.gigamonkeys.binary-data :cl-ppcre :metabang-bind))
(in-package #:elf)

;; required so that :com.gigamonkeys.binary-data can change :common-lisp stuff
(when (ext:package-lock :common-lisp) (setf (ext:package-lock :common-lisp) nil))


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
(defmacro define-elf-dictionary (name num sign dictionary)
  (declare (indent 3))
  `(define-binary-type ,name ()
     (:reader (in)
              (let ((byte (bytes-from in ,num ,sign)))
                (or (cdr (assoc byte ',dictionary)) byte)))
     (:writer (out val)
              (bytes-to out ,num
                        (if (numberp val)
                            val
                            (car (rassoc val ',dictionary))) ,sign))))

;; integers and bytes
(defun parse-addr (str)
  (let ((clean (cond
                 ((and (> (length str) 2)
                       (string= (subseq str 0 2) "0x"))
                  (subseq str 2))
                 ((and (> (length str) 3)
                       (string= (subseq str 1 3) "0x"))
                  (subseq str 3))
                 (t str))))
    (when (scan "^[0-9a-f]+$" clean)
      (parse-integer clean :radix 16))))

(defvar *endian* 'little
  "Controls the endianness of how bytes are read.")

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

(defun bytes-from (in bytes &optional signed-p &aux steps)
  (let ((buf (make-array bytes :element-type '(unsigned-byte 8))))
    (read-sequence buf in)
    (bytes-to-int (coerce buf 'list) signed-p)))

(defun bytes-to (out bytes value &optional signed-p &aux steps)
  (write-sequence (int-to-bytes value bytes signed-p) out))

(define-binary-type unsigned-integer (bytes)
  (:reader (in) (bytes-from in bytes))
  (:writer (out value) (bytes-to out bytes value)))

(define-binary-type signed-integer (bytes)
  (:reader (in) (bytes-from in bytes 'signed))
  (:writer (out value) (bytes-to out bytes value 'signed)))

(define-binary-type char    () (unsigned-integer :bytes 1))
(define-binary-type half    () (unsigned-integer :bytes 2))
(define-binary-type word    () (unsigned-integer :bytes 4))
(define-binary-type sword   () (signed-integer   :bytes 4))
(define-binary-type xword   () (unsigned-integer :bytes 8))
(define-binary-type sxword  () (signed-integer   :bytes 8))
(define-binary-type 32-addr () (unsigned-integer :bytes 4))
(define-binary-type 32-off  () (unsigned-integer :bytes 4))
(define-binary-type 64-addr () (unsigned-integer :bytes 8))
(define-binary-type 64-off  () (unsigned-integer :bytes 8))

;; ELF dictionaries -- ELF header
(define-elf-dictionary elf-type 2 nil
                       ((0 . :none)
                        (1 . :relocatable)
                        (2 . :executable)
                        (3 . :shared-object)
                        (4 . :core)))

(define-elf-dictionary elf-machine 2 nil
                       ((0 . :no-machine)
                        (1 . :at&t-we-32100)
                        (2 . :sun-sparc)
                        (3 . :intel-80386)
                        (4 . :motorola-m68k-family)
                        (5 . :motorola-m88k-family)
                        (7 . :intel-80860)
                        (8 . :mips-r3000-big-endian)))

(define-elf-dictionary elf-version 4 nil
                       ((0 . :invalid) (1 . :current)))

(define-elf-dictionary elf-class 1 nil
                       ((0 . :invalid) (1 . :32-bit) (2 . :64-bit)))

;; section header table
(define-elf-dictionary sh-type 4 nil
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

(define-elf-dictionary sh-flags 4 nil
                       ((1 . :writable)
                        (2 . :allocatable)
                        (4 . :executable)))

;; program header table
(define-elf-dictionary ph-type 4 nil
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
(define-elf-dictionary dyn-tag 4 'signed
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
                        (23 . :jmprel)))


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
   (entry         32-addr)
   (phoff         32-off)
   (shoff         32-off)
   (flags         word)
   (eh-size       half)
   (ph-ent-size   half)
   (ph-num        half)
   (sh-ent-size   half)
   (sh-num        half)
   (sh-str-ind    half)))

(define-binary-class section-header ()
  ((name      word)
   (type      sh-type)
   (flags     sh-flags)
   (address   32-addr)
   (offset    32-off)
   (size      word)
   (link      word)
   (info      word)
   (addralign word)
   (entsize   word)))

(define-binary-class program-header ()
  ((type   ph-type)
   (offset 32-off)
   (vaddr  32-addr)
   (paddr  32-addr)
   (filesz word)
   (memsz  word)
   (flags  word)
   (align  word)))

(define-binary-class elf-sym ()
  ((name  word)
   (value 32-addr)
   (size  word)
   (info  char)
   (other char)
   (shndx half)))

(define-binary-class elf-dyn ()
  ((tag dyn-tag)
   (un (raw-bytes :length 4))))

(defmethod un-type ((dyn elf-dyn))
  (let ((val (list :pltrel :relent :relsz :rpath :soname :syment
                   :strsz :relaent :relasz :pltrelsz :needed))
        (ptr (list :pltgot :hash :strtab :symtab :rela :init
                   :fini :rel :debug :jmprel))
        (ignored (list :null :symbolic :textrel)))
    (cond ((member (tag dyn) val)     :val)
          ((member (tag dyn) ptr)     :ptr)
          ((member (tag dyn) ignored) :ignored))))

(defmethod ptr ((dyn elf-dyn) &aux steps)
  (when (equal (un-type dyn) :ptr) (bytes-to-int (un dyn))))

(defmethod (setf ptr) (new (dyn elf-dyn) &aux steps)
  (if (equal :ptr (un-type dyn))
      (setf (un dyn) (coerce (int-to-bytes new 4) 'vector))
      (error "Can't set ptr for dynamic section of type ~a" (tag dyn))))

(defmethod val ((dyn elf-dyn))
  (when (equal (un-type dyn) :val) (bytes-to-int (un dyn) 'signed)))

(defmethod (setf val) (new (dyn elf-dyn) &aux steps)
  (if (equal :val (un-type dyn))
      (setf (un dyn) (coerce (int-to-bytes new 4 'signed) 'vector))
      (error "Can't set val for dynamic section of type ~a" (tag dyn))))

(defun dynamic-entry (sec)
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
  (error "TODO: setting flags not yet implemented"))

(defmethod alignment ((sec section))
  (if (ph sec) (align (ph sec)) (addralign (sh sec))))

(defmethod (setf alignment) (new (sec section))
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
                                 collect (read-value 'program-header in))))
       sections
       (let ((str-off (offset (nth (sh-str-ind header) section-table)))
             (path (pathname in)))
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
                            collect (read-value 'elf-sym in)))
                        ((string= ".dynamic" name)
                         (file-position in (offset sh))
                         (loop for x from 0 to (1- (/ (size sh) (entsize sh)))
                            collect (read-value 'elf-dyn in)))
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
           (add-part 0 52 :header)      ; add header
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
                  (dolist (sym (data sec)) (write-value 'elf-sym out sym)))
                 ((string= ".dynamic" (name sec))
                  (dolist (dyn (data sec))
                    (write-value 'elf-dyn out dyn)))
                 (t (write-bytes (data sec)))))))
          ((vectorp chunk)              ; raw filler
           (write-bytes chunk))
          ((eq :header chunk)           ; header
           (write-value 'elf-header out header))
          ((eq :section-table chunk)    ; section table
           (mapc (lambda (c) (write-value 'section-header out c)) section-table))
          ((eq :program-table chunk)    ; program table
           (mapc (lambda (c) (write-value 'program-header out c)) program-table)))))))

(defmethod (setf data) (new (sec section))
  "Update the contents of section to new, and update all headers appropriately."
  ;; step through the ordered sections, updating where required
  (let* ((old-length (* (length (data sec))
                        (if (vectorp (data sec)) 1 (entsize (sh sec)))))
         (new-length (* (length new)
                        (if (vectorp new) 1 (entsize (sh sec)))))
         (end-point (+ (offset sec) old-length))
         (delta (- new-length old-length))
         (d delta) ;; a delta to decremented as sections change sizes
         last)
    (setf
     (ordering (elf sec))
     (remove nil
      (mapcar
       (lambda (chunk)
         (if (equal chunk (name sec))
             (setf last (+ (offset sec) new-length))
             (when (and last (> d 0))
               (cond
                 ((stringp chunk)       ; section
                  (let ((sec (named-section (elf sec) chunk)))
                    (when (not (zerop (address (sh sec))))
                      (let ((dyn (dynamic-entry sec)))
                        (when dyn (setf (ptr dyn) (+ (address (sh sec)) d))))
                      (setf (address (sh sec)) (+ (address (sh sec)) d)))
                    (setf (offset sec) last)
                    (setf last (+ (offset sec) (size sec)))))
                 ((vectorp chunk)       ; padding bytes
                  (if (> d (length chunk))
                      (progn (setf d (- d (length chunk)))
                             (setf chunk nil))
                      (progn (setf last (+ last (- (length chunk) d)))
                             (setf chunk (subseq chunk d))
                             (setf d 0))))
                 ((equal :program-table chunk) ; program header
                  (setf (phoff header) (+ (phoff header) d))
                  (setf last (+ (phoff header)
                                (* (ph-num header) (ph-ent-size header)))))
                 ((equal :section-table chunk) ; section header
                  (setf (shoff header) (+ (shoff header) d))
                  (setf last (+ (shoff header)
                                (* (sh-num header) (sh-ent-size header))))))))
         chunk)
       (ordering (elf sec)))))
    ;; update the dynamic symbols used at run time
    (dolist (sym (data (named-section (elf sec) ".dynsym")))
      ;; No way (for now) to tell where in the section the size
      ;; changed, so only update symbols after the section's end.
      (when (and (> (value sym) end-point) (not (zerop delta)))
        (setf (value sym) (+ (value sym) delta))))
    ;; update size and the contents of the section
    (setf (size sec) new-length)
    (set-data new sec)))


;;; Misc functions
(defun named-section (elf name)
  "Return the section in ELF named NAME."
  (first (remove-if (lambda (sec) (not (string= name (name sec)))) (sections elf))))

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
  (when (equal :executable (type (header elf)))
    (shell "chmod" "+x" file))
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
  (format t "~&beg    data              end   ~%")
  (format t "-------------------------------~%")
  (reduce
   (lambda (beg el)
     ((lambda (end)
        (format t "~6a ~18a ~6a~%"
                beg (cond ((stringp el) el) ((vectorp el) :filler) (t el)) end)
        end)
      (+ beg (cond ((stringp el)
                    (let ((sec (named-section elf el)))
                      (if (equal :nobits (type sec))
                          0
                          (length (data sec)))))
                   ((vectorp el) (length el))
                   ((eq :header el) 52)
                   ((eq :section-table el)
                    (* (sh-ent-size (header elf)) (sh-num (header elf))))
                   ((eq :program-table el)
                    (* (ph-ent-size (header elf)) (ph-num (header elf))))))))
   (ordering elf) :initial-value 0))

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

;;; end of elf.lisp
