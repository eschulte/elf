;;; disassemblable --- Extensions for disassemblable ELF files
(in-package :elf)

;;; Disassembly classes and functions
(defclass disassemblable (elf) ())

(defclass objdump (disassemblable) ())

(defclass csurf (disassemblable)
  ((project :initarg :project :accessor project :initform nil)))

(defclass elf-const (disassemblable)
  ((disassembly :initarg :disassembly :accessor disassembly
                :initform (make-hash-table :test 'equal)))
  (:documentation
   "Disassemblable objects with caches holding disassembly by section name."))

(defclass objdump-const (elf-const objdump) ()
  (:documentation "Caching objdump-backed ELF file."))

(defgeneric disassemble-section (disassemblable section)
  (:documentation
   "Return the disassembly of the contents of SECTION in DISASSEMBLABLE.
The contents are returned grouped by function."))

(defmethod disassemble-section :around ((elf elf-const) section-name)
  (with-slots (disassembly) elf
    (or (gethash section-name disassembly)
        (setf (gethash section-name disassembly) (call-next-method)))))


;;; Disassembly functions using objdump from GNU binutils
(defvar objdump-cmd "objdump" "Name of the objdump executable.")

(defun objdump (section)
  (with-temp-file path
    (write-elf (elf section) path)
    (shell (format nil "~a -j ~a -d ~a" objdump-cmd (name section) path))))

(defvar *single-value-objdump-hack* nil
  "Set to non-nil if objdump prints 4-byte values as a single number.")

(defun parse-addresses (lines)
  "Parse addresses from lines of objdump output."
  (mapcar
   (lambda (line)
     (destructuring-bind (address-str bytes-str . disasm-str)
         (split-sequence #\Tab line)
       (list
        (parse-integer (trim address-str) :radix 16 :junk-allowed t)
        ;; bytes
        (let ((raw-bytes (mapcar (lambda (num) (parse-integer num :radix 16))
                                 (split-sequence #\Space (trim bytes-str)))))
          ;; If only 1 byte is returned,
          (if (or (not *single-value-objdump-hack*)
                  (> (length raw-bytes) 1))
              raw-bytes
              ;; then split it into four bytes.
              (mappend (lambda (raw) (coerce (int-to-bytes raw 4) 'list))
                       raw-bytes)))
        ;; disassembled assembly text
        (format nil "~{~a~^ ~}" disasm-str))))
   (remove-if (lambda (line)
                (or (< (length line) 9)
                    (not (equal #\: (aref line 8)))))
              lines)))

(defun objdump-parse (output)
  "Parse the output of `objdump' returning the disassembly by symbol."
  (let ((lines (split-sequence #\Newline output))
        (sec-header (lambda (line)
                      (multiple-value-bind (matchedp matches)
                          (scan-to-strings "^([0-9a-f]+) <(.+)>:$" line)
                        (when matchedp
                          (cons
                           (parse-integer (aref matches 0) :radix 16)
                           (aref matches 1)))))))
    (mapcar #'cons
            (remove nil (mapcar sec-header lines))
            (mapcar #'parse-addresses
                    (cdr (split-sequence-if sec-header lines))))))

(defmethod disassemble-section ((elf objdump) section-name)
  (objdump-parse (objdump (named-section elf section-name))))


;;; Disassembly functions using csurf from GrammaTech
(defvar csurf-cmd "csurf -nogui")

(defvar csurf-script
  (make-pathname
   :directory
   (pathname-directory #.(or *compile-file-truename*
                             *load-truename*
                             *default-pathname-defaults*))
   :name "sections"
   :type "stk"))

(defmethod csurf-ins (project section)
  (multiple-value-bind (stdout stderr errno)
      (shell (format nil "~a ~a -l ~a -- ~a"
                     csurf-cmd project csurf-script section))
    (unless (zerop errno)
      (error "csurf failed with ~s" stderr))
    ;; parse addresses
    (mapcar (lambda (line)
              (multiple-value-bind (matchp matches)
                  (scan-to-strings "^([0-9]+)[\\s]+(.*)$" line)
                (declare (ignorable matchp))
                (cons (parse-integer (aref matches 0))
                      (aref matches 1))))
            (split-sequence #\Newline stdout :remove-empty-subseqs t))))

(defmethod disassemble-section ((elf csurf) section-name &aux last)
  ;; Implementation of disasm using the objdump support provided by
  ;; the ELF library.
  (let ((data (data (named-section elf section-name)))
        (offset (address (sh (named-section elf section-name)))))
    (cdr (mapcar (lambda (pair)
                   (prog1 (when last
                            (list (+ (car last) offset)
                                  (coerce (subseq data (car last) (car pair))
                                          'list)
                                  (cdr last)))
                     (setf last pair)))
                 (append (csurf-ins (project elf) section-name)
                         (list (cons (length data) "NO disasm")))))))
