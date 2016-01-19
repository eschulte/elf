;;; elf.lisp --- A Common Lisp library for manipulating ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; See [ELF.txt](ELF.txt) for more information on the elf format.
;; Much of the code in `elf.lisp` is a direct translation of the elf
;; data structures described in the ELF.txt document augmented with
;; specific information translated from `/usr/include/elf.h`.
;;
;; Example Usage
;; =============
;;
;; load the elf library
;; ---------------------
;; First load the up the `elf` library.
;;
;;     (require :elf)
;;     (in-package :elf)
;;
;; create a simple elf binary and confirm it is an elf file
;; ---------------------------------------------------------
;; For the remainder of this example, we'll use a simple elf binary
;; executable named =hello=, compiled from the following C code.
;;
;;     echo 'main(){puts("hello world");}'|gcc -x c - -o hello
;;
;; We can check that this is indeed an elf file by checking the magic
;; number at the start of the file.
;;
;;     (elf-p "hello") ; => T
;;
;; We can also view an elf files header information without parsing
;; the entire file.
;;
;;     (elf::show-it (elf-header "hello") :out nil)
;;
;; read an elf object, and view it's header information
;; ----------------------------------------------------
;; Then we read the binary file into an elf object.
;;
;;     (defvar *elf* (read-elf "hello"))
;;
;; Using the `show-it` function from the elf package we can inspect
;; the header at the top of the elf file.
;;
;;     (elf::show-it (header *elf*) :out nil)
;;
;; view section-table and program-table information
;; -------------------------------------------------
;; We can list the names of the sections of the elf file.
;;
;;     (mapcar #'name (sections *elf*))
;;     ;; => ("" ".interp" ".note.ABI-tag" ".note.gnu.build-id"...
;;
;; We can list the segments in the program table, and view both the
;; layout of the elements of the elf file, both in it's binary file
;; and when it is an executable image in memory.
;;
;;     ;; looking at the program table
;;     (mapc #'elf::show-it (program-table *elf*))
;;     TYPE:PHDR FLAGS:5 OFFSET:64 VADDR:4194368 PADDR:4194368 FILE...
;;     TYPE:INTERP FLAGS:4 OFFSET:512 VADDR:4194816 PADDR:4194816 F...
;;     TYPE:LOAD FLAGS:5 OFFSET:0 VADDR:4194304 PADDR:4194304 FILES...
;;     TYPE:LOAD FLAGS:6 OFFSET:1744 VADDR:6293200 PADDR:6293200 FI...
;;     TYPE:DYNAMIC FLAGS:6 OFFSET:1768 VADDR:6293224 PADDR:6293224...
;;     TYPE:NOTE FLAGS:4 OFFSET:540 VADDR:4194844 PADDR:4194844 FIL...
;;     TYPE:GNU_EH_FRAME FLAGS:4 OFFSET:1472 VADDR:4195776 PADDR:41...
;;     TYPE:GNU_STACK FLAGS:6 OFFSET:0 VADDR:0 PADDR:0 FILESZ:0 MEM...
;;
;;     ;; view the contents of elf, as they exist in the file
;;     (show-file-layout *elf*)
;;     START    OFFSET   CONTENTS           END
;;     0        NONE     HEADER             64
;;     64       NONE     PROGRAM-TABLE      512
;;     512      512      .interp            540
;;     540      540      .note.ABI-tag      572
;;     572      572      .note.gnu.build-id 608
;;     608      608      .gnu.hash          636
;;     636      NONE     FILLER             640
;;     640      640      .dynsym            736
;;     736      736      .dynstr            797
;;     ...
;;
;;     ;; view the contents of elf, as they exist in the file
;;     (show-memory-layout *elf*)
;;     addr     contents          end
;;     -------------------------------------
;;     0x400000 LOAD               0x4006CC
;;     0x400040 PHDR               0x400200
;;     0x400200 INTERP             0x40021C
;;     0x400200 .interp            0x40021C
;;     0x40021C NOTE               0x400260
;;     0x40021C .note.ABI-tag      0x40023C
;;     ...
;;
;; write an elf object to disk
;; ----------------------------
;; We can write out the elf file to disk.
;;
;;     ;; write out the elf file, the results should be identical to
;;     ;; the original
;;     (write-elf *elf* "hello2")
;;
;; The resulting file will be identical to the original file from
;; which the elf object was read.
;;
;;     diff hello hello2
;;
;; manipulate the contents of an elf object
;; -----------------------------------------
;; We can manipulate these elf objects, and then write the results
;; back out to disk.  For example we can change the code in the
;; `.text` section of the file, and then write the results back out to
;; disk.
;;
;;     ;; change the .text section -- this doesn't break the program
;;     (aref (data (named-section *elf* ".text")) 40) ; => 144
;;     (setf (aref (data (named-section *elf* ".text")) 40) #xc3)
;;     (aref (data (named-section *elf* ".text")) 40) ; => 195
;;
;;     ;; When we write the modified elf to a file, the resulting file
;;     ;; will be different than the original hello (in one byte) but
;;     ;; will still execute since we changed a byte off of the
;;     ;; execution path
;;     (write-elf *elf* "hello2")
;;
;; Meta information like the relevant program and section headers, as
;; well as symbol information in the `.dynamic` section of the file
;; will be automatically updated.
;;
;;     (let ((text (named-section *elf* ".text")))
;;       (setf (data text)
;;             (concatenate 'vector
;;                          (data text)
;;                          (make-array 16 :initial-element #x90))))
;;     (write-elf *elf* "hello3")
;;
;; Note however that the resulting file will segfault on evaluation,
;; because even though the meta-data of the elf file is updated
;; automatically, there are hard-coded offsets and memory locations in
;; the compiled data contained in the elf file, which can not be
;; automatically updated.

;;; Code:
(in-package :elf)


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

(define-binary-type raw-bits (length)
  (:reader (in)
           (let ((buf (make-array length :element-type '(unsigned-byte 1))))
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

(defmacro define-bit-dictionary (name num dictionary)
  `(define-binary-type ,name ()
     (:reader (in)
              (let ((value
                     (bits-to-int
                      (let ((buf (make-array ,num
                                             :element-type '(unsigned-byte 1))))
                        (read-sequence buf in)
                        buf))))
                (or (cdr (assoc value ',dictionary)) value)))
     (:writer (out val)
              (write-sequence
               (int-to-bits (if (numberp val)
                                val
                                (car (rassoc val ',dictionary))) ,num)
               out))))

;; integers and bytes
(defvar *endian* :little
  "Controls the endianness of how bytes are read.")

(defvar *class* nil
  "Word size of the machine, (e.g. :32-bit or :64-bit).")

(define-condition bad-elf-class (error)
  ((class :initarg :class :reader class)))

(defun bytes-to-int (bytes &optional signed-p (byte-size 8) &aux steps)
  (dotimes (n (length bytes)) (setf steps (cons (* n byte-size) steps)))
  (unless (listp bytes) (setf bytes (coerce bytes 'list)))
  (let ((value 0))
    (mapc (lambda (bit b) (setf (ldb (byte byte-size bit) value) b))
          (if (eq *endian* :little) (reverse steps) steps) bytes)
    (if (and signed-p (> value (expt 2 (1- (* byte-size (length bytes))))))
        (- (expt 2 (1- (* byte-size (length bytes)))) value)
        value)))

(defun int-to-bytes (int size &optional signed-p (byte-size 8) &aux steps)
  (dotimes (n size) (setf steps (cons (* n byte-size) steps)))
  (let ((buf (make-array size
                         :element-type `(unsigned-byte ,byte-size)
                         :fill-pointer 0)))
    (when (and signed-p (< int 0))
      (setf int (+ (expt 2 (1- (* byte-size size))) (- 0 int))))
    (mapc (lambda (bit) (vector-push (ldb (byte byte-size bit) int) buf))
          (if (eq *endian* :little) (reverse steps) steps))
    buf))

(defun bits-to-int (bits &optional signed-p)
  (bytes-to-int bits signed-p 1))

(defun int-to-bits (byte size &optional signed-p)
  (int-to-bytes byte size signed-p 1))

(defun bytes-from (in bytes &optional signed-p (byte-size 8))
  (let ((buf (make-array bytes :element-type `(unsigned-byte ,byte-size))))
    (read-sequence buf in)
    (bytes-to-int (coerce buf 'list) signed-p)))

(defun bytes-to (out bytes value &optional signed-p (byte-size 8))
  (write-sequence (int-to-bytes value bytes signed-p byte-size) out))

(define-binary-type unsigned-integer (bytes byte-size)
  (:reader (in) (bytes-from in bytes nil (or byte-size 8)))
  (:writer (out value) (bytes-to out bytes value nil (or byte-size 8))))

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
  ((0  . :none)
   (1  . :m32)
   (2  . :sparc)
   (3  . :386)
   (4  . :68k)
   (5  . :88k)
   (7  . :860)
   (8  . :mips)
   (9  . :s370)
   (10 . :mips_rs3_le)
   (15 . :parisc)
   (17 . :vpp500)
   (18 . :sparc32plus)
   (19 . :960)
   (20 . :ppc)
   (21 . :ppc64)
   (22 . :s390)
   (36 . :v800)
   (37 . :fr20)
   (38 . :rh32)
   (39 . :rce)
   (40 . :arm)
   (41 . :fake_alpha)
   (42 . :sh)
   (43 . :sparcv9)
   (44 . :tricore)
   (45 . :arc)
   (46 . :h8_300)
   (47 . :h8_300h)
   (48 . :h8s)
   (49 . :h8_500)
   (50 . :ia_64)
   (51 . :mips_x)
   (52 . :coldfire)
   (53 . :68hc12)
   (54 . :mma)
   (55 . :pcp)
   (56 . :ncpu)
   (57 . :ndr1)
   (58 . :starcore)
   (59 . :me16)
   (60 . :st100)
   (61 . :tinyj)
   (62 . :x86_64)
   (63 . :pdsp)
   (66 . :fx66)
   (67 . :st9plus)
   (68 . :st7)
   (69 . :68hc16)
   (70 . :68hc11)
   (71 . :68hc08)
   (72 . :68hc05)
   (73 . :svx)
   (74 . :st19)
   (75 . :vax)
   (76 . :cris)
   (77 . :javelin)
   (78 . :firepath)
   (79 . :zsp)
   (80 . :mmix)
   (81 . :huany)
   (82 . :prism)
   (83 . :avr)
   (84 . :fr30)
   (85 . :d10v)
   (86 . :d30v)
   (87 . :v850)
   (88 . :m32r)
   (89 . :mn10300)
   (90 . :mn10200)
   (91 . :pj)
   (92 . :openrisc)
   (93 . :arc_a5)
   (94 . :xtensa)
   (95 . :num)))

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

;; Program Header flag permissions
;;
(defvar pf-x        #x1        "Program header flag execute bit.")
(defvar pf-w        #x2        "Program header flag write bit.")
(defvar pf-r        #x3        "Program header flag read bit.")
(defvar pf-maskos   #x0ff00000 "Program header flag Unspecified bit.")
(defvar pf-maskproc #xf0000000 "Program header flag Unspecified bit.")
;;
;; Possible values and meanings.
;;
;; | Flags          | Value | Exact                | Allowable            |
;; |----------------+-------+----------------------+----------------------|
;; | none           |     0 | All access denied    | All access denied    |
;; | PF_X           |     1 | Execute only         | Read, execute        |
;; | PF_W           |     2 | Write only           | Read, write, execute |
;; | PF_W+PF_X      |     3 | Write, execute       | Read, write, execute |
;; | PF_R           |     4 | Read only            | Read, execute        |
;; | PF_R+PF_X      |     5 | Read, execute        | Read, execute        |
;; | PF_R+PF_W      |     6 | Read, write          | Read, write, execute |
;; | PF_R+PF_W+PF_X |     7 | Read, write, execute | Read, write, execute |

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

(defgeneric rel-sym (rel)
  (:documentation "Shift bits in REL based on its class."))
(defmethod rel-sym ((rel elf-rel))
  (ash (info rel)
       (ecase (class-name (class-of rel))
         ((elf-rel-32 elf-rela-32) -8)
         ((elf-rel-64 elf-rela-64) -32))))

(defvar rel-types
  '((:386
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
     (10 . :gotpc))
    (:x86_64
     (0  . :none)
     (1  . :32)
     (2  . :pc32)
     (3  . :got32)
     (4  . :plt32)
     (5  . :copy)
     (6  . :glob_dat)
     (7  . :jmp_slot)
     (8  . :relative)
     (9  . :gotoff)
     (10 . :gotpc)
     (11 . :32plt)
     (14 . :tls_tpoff)
     (15 . :tls_ie)
     (16 . :tls_gotie)
     (17 . :tls_le)
     (18 . :tls_gd)
     (19 . :tls_ldm)
     (20 . :16)
     (21 . :pc16)
     (22 . :8)
     (23 . :pc8)
     (24 . :tls_gd_32)
     (25 . :tls_gd_push)
     (26 . :tls_gd_call)
     (27 . :tls_gd_pop)
     (28 . :tls_ldm_32)
     (29 . :tls_ldm_push)
     (30 . :tls_ldm_call)
     (31 . :tls_ldm_pop)
     (32 . :tls_ldo_32)
     (33 . :tls_ie_32)
     (34 . :tls_le_32)
     (35 . :tls_dtpmod32)
     (36 . :tls_dtpoff32)
     (37 . :tls_tpoff32)
     (39 . :tls_gotdesc)
     (40 . :tls_desc_call)
     (41 . :tls_desc)
     (42 . :irelative)
     (43 . :num)))
  "Association list of type meaning by machine type.")

(defgeneric rel-type (rel header)
  (:documentation "The interpretation of the type is machine specific."))

(defmethod rel-type ((rel elf-rel) (header elf-header))
  (let ((val (logand (info rel)
                     (ecase (class-name (class-of rel))
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

(defun name-symbols (sec)
  "Assign names to the symbols contained in SEC."
  (when (member (type sec) '(:symtab :dynsym))
    (with-slots (elf sh type) sec
      (let* ((name-sec (nth (link sh) (sections elf)))
             (tab (coerce (data name-sec) 'list)))
        (mapcar (lambda (sym)
                  (setf (sym-name sym)
                        (coerce (loop for code in (subseq tab (name sym))
                                   until (equal code 0)
                                   collect (code-char code))
                                'string)))
                (data sec))))))

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

;;; TODO: implement a type for elements of the .plt table
;;;       (similar to what's currently used to parse data for .dynsym)

(defgeneric word-size (dyn)
  (:documentation "Return the word size for the given class."))

(defmethod word-size ((dyn elf-dyn-32)) 4)
(defmethod word-size ((dyn elf-dyn-64)) 8)

(defgeneric un-type (dyn))
(defmethod un-type ((dyn elf-dyn))
  (let ((val (list :pltrel :relent :relsz :rpath :soname :syment
                   :strsz :relaent :relasz :pltrelsz :needed))
        (ptr (list :pltgot :hash :strtab :symtab :rela :init
                   :fini :rel :debug :jmprel))
        (ignored (list :null :symbolic :textrel)))
    (cond ((member (tag dyn) val)     :val)
          ((member (tag dyn) ptr)     :ptr)
          ((member (tag dyn) ignored) :ignored))))

(defgeneric ptr (dyn))
(defmethod ptr ((dyn elf-dyn))
  (when (equal (un-type dyn) :ptr) (bytes-to-int (un dyn))))

(defgeneric (setf ptr) (new dyn))
(defmethod (setf ptr) (new (dyn elf-dyn))
  (if (equal :ptr (un-type dyn))
      (setf (un dyn) (coerce (int-to-bytes new (word-size dyn)) 'vector))
      (error "Can't set ptr for dynamic section of type ~a" (tag dyn))))

(defgeneric val (dyn))
(defmethod val ((dyn elf-dyn))
  (when (equal (un-type dyn) :val) (bytes-to-int (un dyn) 'signed)))

(defgeneric (setf val) (new dyn))
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

(defgeneric binding (sym))
(defmethod binding ((symbol elf-sym))
  (ecase (ash (info symbol) -4)
    (0  :LOCAL)
    (1  :GLOBAL)
    (2  :WEAK)
    (13 :LOPROC)
    (15 :HIPROC)))

(defmethod (setf binding) (new (symbol elf-sym))
  (setf (info symbol)
        (+ (ash (ecase new
                  (:LOCAL  0)
                  (:GLOBAL 1)
                  (:WEAK   2)
                  (:LOPROC 13)
                  (:HIPROC 15)) 4)
           (logand (info symbol) #xf))))

(defmethod type ((symbol elf-sym))
  (ecase (logand (info symbol) #xf)
    (0  :NOTYPE)
    (1  :OBJECT)
    (2  :FUNC)
    (3  :SECTION)
    (4  :FILE)
    (13 :LOPROC)
    (15 :HIPROC)))

(defmethod (setf type) (new (symbol elf-sym))
  (setf (info symbol)
        (+ (ash (info symbol) -4)
           (logand (ecase new
                     (:NOTYPE  0)
                     (:OBJECT  1)
                     (:FUNC    2)
                     (:SECTION 3)
                     (:FILE    4)
                     (:LOPROC  13)
                     (:HIPROC  15)) #xf))))

(defclass section ()
  ((elf  :initarg :elf  :accessor elf)
   (sh   :initarg :sh   :accessor sh)
   (ph   :initarg :ph   :accessor ph)
   (name :initarg :name :accessor name)
   (data :initarg :data :reader data :writer set-data)))

(defmethod offset ((sec section))
  (offset (or (sh sec) (ph sec))))

(defmethod (setf offset) (new (sec section))
  (if (sh sec)
      (progn
        (setf (offset (sh sec)) new)
        (when (and (ph sec) (= (offset sec) (offset (ph sec))))
          (setf (offset (ph sec)) new)))
      (setf (offset (ph sec)) new)))

(defgeneric vma (section)
  (:documentation "Return the virtual memory address for SECTION."))
(defmethod vma ((sec section))
  (when (ph sec) (+ (vaddr (ph sec)) (- (offset sec) (offset (ph sec))))))

(defgeneric (setf vma) (new section))
(defmethod (setf vma) (new (sec section))
  (declare (ignorable new))
  (error "Don't set the VMA, this is calculated from the ph and offset."))

(defmethod size ((sec section))
  (if (sh sec) (size (sh sec)) (filesz (ph sec))))

(defmethod (setf size) (new (sec section))
  (let ((delta (- new (size sec))))
    (when (sh sec)
      (setf (size (sh sec)) new))
    (when (ph sec)
      ;; TODO: probably need a better test here at some point
      (when (= (filesz (ph sec)) (memsz (ph sec)))
        (setf (memsz (ph sec)) (+ delta (memsz (ph sec)))))
      (setf (filesz (ph sec)) (+ delta (filesz (ph sec)))))))

(defmethod type ((sec section))
  (type (or (sh sec) (ph sec))))

(defmethod (setf type) (new (sec section))
  (if (sh sec)
      (setf (type (sh sec)) new)
      (setf (type (ph sec)) new)))

(defmethod flags ((sec section))
  (flags (concatenate 'list (ph sec) (sh sec))))

(defmethod (setf flags) (new (sec section))
  (declare (ignorable new))
  (error "TODO: setting flags not yet implemented"))

(defgeneric alignment (section))
(defmethod alignment ((sec section))
  (if (ph sec) (align (ph sec)) (addralign (sh sec))))

(defgeneric (setf alignment) (new section))
(defmethod (setf alignment) (new (sec section))
  (declare (ignorable new))
  (error "TODO: setting alignment is not yet implemented"))

(defclass elf ()
  ((header        :initarg :header        :accessor header)
   (section-table :initarg :section-table :accessor section-table)
   (program-table :initarg :program-table :accessor program-table)
   ;; This list holds the actual data sections of the ELF file.  This
   ;; list is populated using the section-table if it exists or the
   ;; program-table otherwise.
   (sections      :initarg :sections      :accessor sections)
   ;; The ordering list is used to write a modified ELF file back to
   ;; disk.  It includes the header, section-table, and program-table,
   ;; as well as all sections which contain data which lives in the
   ;; ELF file.
   ;;
   ;; Every element of ordering is of the form (offset size value)
   ;; where offset is the offset in the file at which the value should
   ;; be written, size indicates the size of the value, and value
   ;; indicates the value to be written.  Value may be :header,
   ;; :section-table or :program-table to write the respective headers
   ;; or an index into the sections list to write the data of that
   ;; section.
   ;;
   ;; In some cases ELF files have sections with overlapping
   ;; representations in the file (e.g., because some program data
   ;; must start at offset 0 overlapping the elf header).  In these
   ;; cases last section written will override previously written
   ;; sections, and the sections are written in the order they appear
   ;; in the ordering list.
   ;;
   ;; TODO: In those cases where multiple sections point to the same
   ;;       file data, only one copy of the file data should exist,
   ;;       and each section should point to the same copy (s.t.,
   ;;       setting that data in one section will change it in all
   ;;       sections).  Ideally more specialized sections should have
   ;;       priority in setting data.
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
  (elf-read-value type in))

(defmethod read-value ((type (eql 'objdump)) in &key)
  (elf-read-value type in))

(defmethod read-value ((type (eql 'csurf)) in &key)
  (elf-read-value type in))

(defmethod read-value ((type (eql 'elf-const)) in &key)
  (elf-read-value type in))

(defmethod read-value ((type (eql 'objdump-const)) in &key)
  (elf-read-value type in))

(defun elf-read-value (type in)
  ;; Read an elf object from a binary input stream.
  (flet ((raw-bytes (from size)
           (file-position in from)
           (read-value 'raw-bytes in :length size))
         (range-minus (from take)
           (cond
             ((or (< (cdr take) (car from))   ; no overlap
                  (> (car take) (cdr from)))
              (list from))
             ((and (<= (car take) (car from)) ; from front
                   (<= (cdr take) (cdr from)))
              (list (cons (cdr take) (cdr from))))
             ((and (>= (cdr take) (cdr from)) ; from back
                   (>= (car take) (car from)))
              (list (cons (car from) (car take))))
             ((and (>= (car take) (car from)) ; from middle
                   (<= (cdr take) (cdr from)))
              (list (cons (car from) (car take))
                    (cons (cdr take) (cdr from))))
             ((and (<= (car take) (car from)) ; complete overlap
                   (>= (cdr take) (cdr from)))
              nil)
             (:otherwise (error "bad range-minus ~S" (list from take))))))
    (let ((e (make-instance type)))
      (with-slots (header section-table program-table sections ordering) e
        (setf header (elf-header-endianness-warn (read-value 'elf-header in))
              program-table
              (unless (zerop (phoff header))
                (progn (file-position in (phoff header))
                       (loop for x from 0 to (1- (ph-num header))
                          collect
                            (read-value (program-header-type) in))))
              section-table
              (unless (zerop (shoff header))
                (progn (file-position in (shoff header))
                       (loop for x from 0 to (1- (sh-num header))
                          collect (read-value 'section-header in)))))
        ;; initialize the list of sections
        (let ((str-off (when (and section-table (sh-str-ind header))
                         (offset (nth (sh-str-ind header) section-table)))))
          (flet ((header-to-section (h)
                   (let ((section (make-instance 'section)))
                     (with-slots (elf sh ph name data) section
                       (setf elf e)
                       (if section-table
                           (setf sh h
                                 ph (program-header-for-section
                                     program-table section))
                           (setf sh nil
                                 ph h))
                       (setf name
                             (when str-off
                               (progn (file-position in (+ str-off (name h)))
                                      (read-value 'terminated-string in))))
                       (setf
                        data ;; data can vary based on the specific section
                        (cond
                          ((or (string= ".dynsym" name)
                               (string= ".symtab" name))
                           (file-position in (offset h))
                           (loop for x from 0 to (1- (/ (size h) (entsize h)))
                              collect (read-value (elf-sym-type) in)))
                          ((string= ".dynamic" name)
                           (file-position in (offset h))
                           (loop for x from 0 to (1- (/ (size h) (entsize h)))
                              collect (read-value (elf-dyn-type) in)))
                          ((equal :rel (type h))
                           (file-position in (offset h))
                           (loop for x from 0 to (1- (/ (size h) (entsize h)))
                              collect (read-value (elf-rel-type) in)))
                          ((equal :rela (type h))
                           (file-position in (offset h))
                           (loop for x from 0 to (1- (/ (size h) (entsize h)))
                              collect (read-value (elf-rela-type) in)))
                          (t (raw-bytes (offset h) (size section))))))
                     section)))
            (setf sections (mapcar #'header-to-section
                                   (or section-table program-table)))))
        ;; compile the ordering of the sections
        (let* ((parts (append
                       ;; sections
                       (remove-if (lambda-bind ((offset size num))
                                    (declare (ignorable num))
                                    ;; In the section table inconsequential
                                    ;; sections are indicated by having an
                                    ;; offset of 0, while in the program
                                    ;; table an offset of 0 is common, but
                                    ;; sections with 0 filesz may be ignored.
                                    (zerop (if section-table offset size)))
                                  (mapcar (lambda-bind ((num sec))
                                            (list (offset sec) (size sec) num))
                                          (indexed sections)))
                       ;; special parts
                       (list (list 0 (elf-header-size) :header))
                       (unless (zerop (shoff header))
                         (list (list (shoff header)
                                     (* (sh-num header) (sh-ent-size header))
                                     :section-table)))
                       (unless (zerop (phoff header))
                         (list (list (phoff header)
                                     (* (ph-num header) (ph-ent-size header))
                                     :program-table)))
                       (list (list (file-length in) 0 :end))))
               (filler
                (mapcar
                 (lambda-bind ((start . end))
                   (list start (- end start) (raw-bytes start (- end start))))
                 (remove-if
                  (lambda-bind ((start . end)) (= start end))
                  (reduce (lambda (left this)
                            (mapcan (lambda (range) (range-minus range this)) left))
                          (mapcar (lambda-bind ((off size data))
                                    (declare (ignorable data))
                                    (cons off (+ off size)))
                                  parts)
                          :initial-value (list (cons 0 (file-length in))))))))
          (setf ordering (sort (append parts filler) #'< :key #'first))))
      ;; initialize symbol names in .symtab and .dynsym
      (mapcar #'name-symbols
              (remove nil
                (mapcar (lambda (name) (named-section e name))
                        '(".symtab" ".dynsym"))))
      e)))

(defmethod write-value ((type (eql 'elf)) out value &key
                        &aux (data (make-array
                                    (caar (remove-if-not
                                           (lambda (g) (eql (lastcar g) :end))
                                           (ordering value)))
                                    :element-type '(unsigned-byte 8)
                                    :initial-element #x0)))
  ;; Write an elf object to a binary output stream.
  (with-slots (header section-table program-table sections ordering) value
    (macrolet
        ((bytes (&rest body)
           `(with-output-to-sequence (o :element-type '(unsigned-byte 8))
              ,@body)))
      (mapc (lambda-bind ((offset . bytes))
              (loop :for n :below (length bytes)
                 :do (setf (aref data (+ offset n)) (aref bytes n))))
            (mapcar
             (lambda-bind ((offset size data))
               (declare (ignorable size))
               (cons
                offset
                (cond
                  ((numberp data)              ; numbered section
                   (let ((sec (nth data (sections value))))
                     (unless (equal :nobits (type sec))
                       (cond
                         ((or (string= ".dynsym" (name sec))
                              (string= ".symtab" (name sec)))
                          (bytes (dolist (sym (data sec))
                                   (write-value (elf-sym-type) o sym))))
                         ((string= ".dynamic" (name sec))
                          (bytes (dolist (dyn (data sec))
                                   (write-value (elf-dyn-type) o dyn))))
                         ((equal :rel (type sec))
                          (bytes (dolist (rel (data sec))
                                   (write-value (elf-rel-type) o rel))))
                         ((equal :rela (type sec))
                          (bytes (dolist (rel (data sec))
                                   (write-value (elf-rela-type) o rel))))
                         (t (data sec))))))
                  ((eq :header data)        ; header
                   (bytes (write-value 'elf-header o header)))
                  ((eq :section-table data) ; section table
                   (bytes (mapc (lambda (c) (write-value 'section-header o c))
                                section-table)))
                  ((eq :program-table data) ; program table
                   (bytes (mapc (lambda (c) (write-value (program-header-type) o c))
                                program-table)))
                  ((vectorp data) data)))) ; raw filler
             ordering))))
  (dolist (b (coerce data 'list)) (write-byte b out)))

(defun copy-elf (elf)
  (unless (member (class-name (class-of elf))
                  '(elf objdump csurf))
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

(defgeneric (setf data) (new section)
  (:documentation
   "Update the contents of section to new, and update all headers appropriately."))
(defmethod (setf data) (new (sec section))
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
                    ;; TODO: when .got or .got.plt sections move, the
                    ;; related offsets in relocation sections will
                    ;; need to be updated
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
      #+do-dyn-update ;; TODO: check if this ever /causes/ execution errors
      (let ((ds (make-array (length data) :initial-element 0)))
        (flet ((adj (address)
                 (+ address
                    (if (and (>= address (offset sec))
                             (<= address (+ (offset sec) (size sec))))
                        ;; inside of the changed section
                        (aref ds (- address (offset sec)))
                        ;; after the changed section
                        (or (cdr (assoc-if (lambda (p) (> address p)) sec-deltas))
                            0)))))
          (dolist (sym (data (named-section elf ".dynsym")))
            (with-slots (value) sym
              (setf value (adj value))))
          (dolist (dynsym (data (named-section elf ".dynamic")))
            (when (ptr dynsym)
              (setf (ptr dynsym) (adj (ptr dynsym)))))))
      ;; update size and the contents of the section
      (setf (size sec) new-length)
      (set-data new sec))))


;;; Misc functions
(defun named-section (elf name)
  "Return the section in ELF named NAME."
  (first (remove-if (lambda (sec) (not (string= name (name sec)))) (sections elf))))

(defgeneric symbols (elf)
  (:documentation "Return the symbols contained in ELF."))
(defmethod symbols ((elf elf))
  (data (named-section elf ".symtab")))

(defun named-symbol (elf name)
  "Return the symbol in ELF named NAME."
  (first (remove-if (lambda (sec) (not (string= name (sym-name sec))))
                    (symbols elf))))

(defvar elf-magic-numbers
  '((#\Rubout #\E #\L #\F)
    (#\Rubout #\C #\G #\C)))

(defun elf-p (file)
  "Return t if file is an ELF file (using the magic number test)."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (car (member (coerce (read-value 'string in :length 4) 'list)
                 elf-magic-numbers :test #'equalp))))

(defun elf-header-endianness-warn (header)
  "Raise a warning if HEADER was read using the wrong endianness."
  (when (if (eq *endian* :little)
            (= (data-encoding header) 2)
            (= (data-encoding header) 1))
    (warn "Header read with wrong encoding ~S while data-encoding ~S."
          *endian* (data-encoding header)))
  header)

(defun elf-header (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (elf-header-endianness-warn (read-value 'elf-header in))))

(defun read-elf (file &optional (type 'elf))
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value type in)))

(defun write-elf (elf file)
  (with-open-file (out file :direction :output :element-type '(unsigned-byte 8))
    (write-value 'elf out elf))
  nil)

(defun show-rel (elf)
  "Print information on the symbols stored in a relocatable section.
Note: the output should resemble the output of readelf -r."
  (let ((sec-f "~&~%Relocation section '~a' at offset 0x~x contains ~a entries:")
        (rel-h "~&  Offset          Info        Type       Sym. Name")
        (rel-f "~&~12,'0x ~12,'0x ~12:@<~a~> ~12:@<~a~>"))
    (with-slots (header) elf
      (dolist (sec (sections elf))
        (when (member (type sec) '(:rel :rela))
          (with-slots (sh name data) sec
            (let ((syms (symbols elf)))
              (format t sec-f name (offset sh) (length data))
              (format t rel-h)
              (mapcar
               (lambda (rel) ;; offset info type sym.name
                 (format t rel-f
                         (offset rel)
                         (info rel)
                         (rel-type rel header)
                         (sym-name (nth (rel-sym rel) syms))))
               data))))))))

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
          (dynstr (let ((dynstr (named-section elf ".dynstr")))
                    (when dynstr
                      (mapcar #'code-char (coerce (data dynstr) 'list)))))
          (symtab (named-section elf ".symtab"))
          (strtab (mapcar #'code-char
                          (coerce (data (named-section elf ".strtab")) 'list))))
      (dolist (tab (list dynsym symtab))
        (when tab
          (format t "~&~%Symbol table '~a' contains ~d entries:~%"
                  (name tab) (length (data tab)))
          (format t "   Num:    Value  Size Type     Bind  Name~%")
          (loop for sym in (data tab) as i from 0
             do (format t "~6d: ~8x ~5d ~8a ~6a ~a~%"
                        i (value sym) (size sym) (type sym) (binding sym)
                        (string-at
                         (if (string= ".dynstr" (name tab)) dynstr strtab)
                         (name sym)))))))))

(defun show-file-layout (elf)
  "Show the layout of the elements of an elf file with binary offset."
  (let ((layout
         (mapcar (lambda-bind ((offset size data))
                   (list offset
                         ;; an identifier for the section data
                         (cond
                           ((numberp data) (name (nth data (sections elf))))
                           ((stringp data) data)
                           ((vectorp data) :filler)
                           (t data))
                         ;; the size in the file
                         (let ((sec (cond
                                      ((numberp data)(nth data (sections elf)))
                                      ((stringp data) (named-section elf data))
                                      (t nil))))
                           (+ offset (if (and sec (equal :nobits (type sec)))
                                         0
                                         size)))))
                 (ordering elf))))
    (format t "~:{~&~8a ~18a ~8a~}~%" (cons (list 'offset 'contents 'end)
                                            layout))))

(defun memory-sorted-sections (elf)
  "Return the sections of the ELF file sorted by their order in memory.
Each element of the resulting list is a triplet of (offset size header)."
  (with-slots (sections section-table program-table) elf
    (stable-sort
     (remove-if
      #'null
      (append
       (mapcar (lambda (head)
                 (when (not (zerop (vaddr head)))
                   (list (vaddr head) (memsz head) head)))
               program-table)
       (when section-table
         (mapcar (lambda (sec) 
                   (when (or (not (zerop (address (sh sec))))
                             (and (zerop (address (sh sec)))
                                  (member (flags (sh sec))
                                          '(:allocatable))
                                  (equal :progbits (type (sh sec)))))
                     (list (address (sh sec)) (size sec) sec)))
                 sections))))
     #'< :key #'car)))

(defun show-memory-layout (elf)
  "Show the layout of the elements of an elf file with binary offset."
  (format t "~&addr     contents          end     ~%")
  (format t "-------------------------------------~%")
  (mapc
   (lambda (trio)
     (bind (((beg size header) trio))
       (format t "~&0x~x ~18a 0x~x~%"
               beg
               (cond
                 ((subtypep (type-of header) (program-header-type))
                  (type header))
                 ((subtypep (type-of header) 'section)
                  (name header)))
               (+ beg size))))
   (memory-sorted-sections elf))
  nil)

(defgeneric file-offset-of-ea (elf ea)
  (:documentation "Return the file offset in ELF of EA."))

(defmethod file-offset-of-ea ((obj elf) ea)
  (let ((section (section-holding-ea obj ea)))
    (+ (offset (sh section)) (- ea (address (sh section))))))


;;; Modification functions
(defmethod index-of-ea ((obj section) ea)
  (- ea (address (sh obj))))

(defun sections-holding-ea (elf ea-start &optional ea-end)
  (remove-if-not
   (lambda (sec)
     (let* ((beg (address (sh sec)))
            (end (+ beg (size sec))))
       (and (>= ea-start beg)
            (< ea-start end)
            (or (not ea-end) (< ea-end end)))))
   (sections elf)))

(defun section-holding-ea (obj ea-start &optional ea-end)
  (let ((sections (sections-holding-ea obj ea-start ea-end)))
    (assert sections (obj ea-start ea-end)
            "No section in ~a found to contain [~a,~a]" obj ea-start ea-end)
    (assert (= 1 (length sections)) (obj ea-start ea-end)
            "Region [~a,~a] spand multiple sections in ~a" obj ea-start ea-end)
    (first sections)))

(defmethod subseq-ea ((obj elf) ea-start &optional ea-end)
  (subseq-ea (section-holding-ea obj ea-start ea-end) ea-start ea-end))

(defmethod (setf subseq-ea) (new (obj elf) ea-start &optional ea-end)
  (setf (subseq-ea (section-holding-ea obj ea-start) ea-start ea-end) new))

(defmethod subseq-ea ((obj section) ea-start &optional ea-end)
  (let ((base (address (sh obj))))
    (subseq (data obj) (- ea-start base) (when ea-end (- ea-end base)))))

(defmethod (setf subseq-ea) (new (obj section) ea-start &optional ea-end)
  (let ((base (address (sh obj))))
    (setf (subseq (data obj) (- ea-start base) (when ea-end (- ea-end base)))
          new)))

(defmethod word-at-ea ((obj section) ea)
  (subseq-ea obj ea (+ 4 ea)))

(defmethod word-at-ea ((obj elf) ea)
  (subseq-ea (section-holding-ea obj ea) ea (+ 4 ea)))

(defmethod (setf word-at-ea) (new (obj section) ea)
  (setf (subseq-ea obj ea (+ 4 ea)) new))

(defmethod (setf word-at-ea) (new (obj elf) ea)
  (setf (subseq-ea (section-holding-ea obj ea) ea (+ 4 ea)) new))

(defgeneric insert (obj data ea)
  (:documentation "Write DATA into OBJ at EA overwriting previous contents."))

(defmethod insert ((obj section) data ea)
  (setf (subseq-ea obj ea (+ ea (length data))) data))

(defmethod insert ((obj elf) data ea)
  (setf (subseq-ea obj ea (+ ea (length data))) data))

(defmethod bits-at-ea ((obj section) ea)
  (int-to-bits (bytes-to-int (subseq-ea obj ea (+ 4 ea))) (* 4 8)))

(defmethod bits-at-ea ((obj elf) ea)
  (int-to-bits (bytes-to-int
                (subseq-ea (section-holding-ea obj ea) ea (+ 4 ea))) (* 4 8)))

(defmethod (setf bits-at-ea) (new (obj section) ea)
  (let ((bytes (int-to-bytes (bits-to-int new) (/ (length new) 8))))
    (setf (subseq-ea obj ea (+ (length bytes) ea)) bytes)))

(defmethod (setf bits-at-ea) (new (obj elf) ea)
  (let ((bytes (int-to-bytes (bits-to-int new) (/ (length new) 8))))
    (setf (subseq-ea (section-holding-ea obj ea)
                     ea (+ (length bytes) ea)) bytes)))

;; Switching from memory reference to file reference
(defun sections-holding-off (elf off-start &optional off-end)
  "Return the section holding the requested file locations.
Note, this does not return filler if the requested file location is
just filler bytes."
  (mapcar (lambda-bind ((beg size value))
            (declare (ignorable beg size))
            (nth value (sections elf)))
          (remove-if-not
           (lambda-bind ((beg size value))
             (let ((end (+ beg size)))
               (and (numberp value)
                    (>= off-start beg)
                    (<= off-start end)
                    (or (not off-end) (< off-end end)))))
           (ordering elf))))

(defun section-holding-off (obj off-start &optional off-end)
  (let ((sections (sections-holding-off obj off-start off-end)))
    (assert sections (obj off-start off-end)
            "No section in ~a found to contain [~a,~a]" obj off-start off-end)
    ;; When multiple sections holds this value, return the last (this
    ;; is the one which will overwrite the others when saving).
    (lastcar sections)))

(defmethod subseq-off ((obj elf) off-start &optional off-end)
  (subseq-off (section-holding-off obj off-start off-end) off-start off-end))

(defmethod (setf subseq-off) (new (obj elf) off-start &optional off-end)
  (setf (subseq-off (section-holding-off obj off-start) off-start off-end) new))

(defmethod subseq-off ((obj section) off-start &optional off-end)
  (let ((base (offset (sh obj))))
    (subseq (data obj) (- off-start base) (when off-end (- off-end base)))))

(defmethod (setf subseq-off) (new (obj section) off-start &optional off-end)
  (let ((base (offset (sh obj))))
    (setf (subseq (data obj) (- off-start base) (when off-end (- off-end base)))
          new)))

(defmethod word-at-off ((obj section) off)
  (subseq-off obj off (+ 4 off)))

(defmethod word-at-off ((obj elf) off)
  (subseq-off (section-holding-off obj off) off (+ 4 off)))

(defmethod (setf word-at-off) (new (obj section) off)
  (setf (subseq-off obj off (+ 4 off)) new))

(defmethod (setf word-at-off) (new (obj elf) off)
  (setf (subseq-off (section-holding-off obj off) off (+ 4 off)) new))
