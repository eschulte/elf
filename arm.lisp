;;; arm.lisp --- Support for reading and writing ARM instructions

;;; Commentary.
;;
;; This file provides methods for reading/writing ARM instructions
;; from/to streams of bits.  It is implemented largely from the
;; ARM7-TDMI-manual-pt2.pdf and page numbers in comments below refer
;; to said.
;; 
;; TODO:
;; - nice print methods so that these instructions are easily readable
;;   at the REPL
;; - instruction disambiguation (see `arm-decode' below)

;;; Code.
(in-package :elf)

(defclass arm-data () ())
(defclass arm-instruction (arm-data) ())

(defmethod initialize-instance :after ((obj arm-instruction) &key)
  (setf (ignored obj) nil))

(defmethod from-bits ((obj arm-data) bits)
  (setf obj (read-value (type-of obj) (make-in-memory-input-stream bits))))

(defmethod from-bytes ((obj arm-data) bytes)
  (from-bits obj (apply #'concatenate 'bit-vector
                        (mapcar (lambda (byte)
                                  (int-to-bits (bytes-to-int (list byte)) 8))
                                bytes))))

(defmethod to-bits ((obj arm-data))
  (with-output-to-sequence (out :element-type '(unsigned-byte 1))
    (write-value (type-of obj) out obj)))

(defmethod to-bytes ((obj arm-data))
  (mapcar (lambda (bits) (int-to-bytes (bits-to-int bits) 1))
          (chunks (to-bits obj) 8)))


;;; Constituents
(define-bit-dictionary condition-field 4
  ;; Condition Field p.5
  ((0  . :eq)                           ; equal
   (1  . :ne)                           ; not equal
   (2  . :cs)                           ; unsigned higher or same
   (3  . :cc)                           ; unsigned lower 
   (4  . :mi)                           ; negative
   (5  . :pl)                           ; positive or zero
   (6  . :vs)                           ; overflow
   (7  . :vc)                           ; no overflow
   (8  . :hi)                           ; unsigned higher
   (9  . :ls)                           ; unsigned lower or same
   (10 . :ge)                           ; greater or equal
   (11 . :lt)                           ; less than
   (12 . :gt)                           ; greater than
   (13 . :le)                           ; less than or equal
   (14 . :al)                           ; always
   (15 . :err)))                        ; never used

(define-bit-dictionary immediate 1
  ((0 . :register) (1 . :immediate)))

(define-bit-dictionary pre/post 1
  ((0 . :post) (1 . :pre)))

(define-bit-dictionary up/down 1
  ((0 . :down) (1 . :up)))

(define-bit-dictionary byte/word 1
  ((0 . :word) (1 . :byte)))

(define-bit-dictionary write-back 1
  ((0 . :no-write-back) (1 . :write-back)))

(define-bit-dictionary load/store 1
  ((0 . :store) (1 . :load)))

(define-bit-dictionary psr 1
  ((0 . :no-psr) (1 . :psr)))

(define-bit-dictionary link-bit 1
  ((0 . :branch) (1 . :branch-w-link)))

(define-bit-dictionary set-condition-codes 1
  ((0 . :no-set) (1 . :set)))

(define-bit-dictionary opcodes 4
  ((0  . :AND)
   (2  . :SUB)
   (3  . :RSB)
   (4  . :ADD)
   (5  . :ADC)
   (6  . :SBC)
   (7  . :RSC)
   (8  . :TST)
   (9  . :TEQ)
   (10 . :CMP)
   (11 . :CMN)
   (12 . :ORR)
   (13 . :MOV)
   (14 . :BIC)
   (15 . :MVN)
   (1  . :EOR)))

(define-binary-type register ()
  (:reader (in)
           (bits-to-int
            (let ((buf (make-array 4 :element-type '(unsigned-byte 1))))
              (read-sequence buf in)
              buf)))
  (:writer (out value)
           (write-sequence (int-to-bits value 4) out)))

(define-binary-type register-list () (raw-bits :length 16))

(define-binary-type forced (value)
  (:reader (in)
           (let ((buf (make-array (length value)
                                  :element-type '(unsigned-byte 1))))
             (read-sequence buf in)
             ;; TODO: Should define a special disasmbly error instead
             ;;       of using assertions.
             (unless (equal value buf)
               (warn "Bits ~a do not match SPEC mandated ~a when decoding ~a"
                     buf value (type-of (car *in-progress-objects*))))
             buf))
  (:writer (out ignore) (declare (ignore ignore)) (write-sequence value out)))

(define-binary-type numerical-value (length)
  (:reader (in)
           (bits-to-int
            (let ((buf (make-array length :element-type '(unsigned-byte 1))))
              (read-sequence buf in)
              buf)))
  (:writer (out value) (write-sequence (int-to-bits value length) out)))


;;; ARM Instructions
(define-binary-class ldr/str (arm-instruction)
  ;; LDR/STR opcodes p. 28
  ;; 31       28                             19      15      11          0
  ;; [ Cond 4 ] [01] [I] [P] [U] [B] [W] [L] [ Rn 4] [ Rd 4] [ Offset 12 ]
  ((offset    (numerical-value :length 12))
   (rd         register)
   (rn         register)
   (l          load/store)
   (w          write-back)
   (b          byte/word)
   (u          up/down)
   (p          pre/post)
   (i          immediate)
   (ignored   (forced :value #*10))
   (conditions condition-field)))

(define-binary-class ldm/stm (arm-instruction)
  ;; LDM/STM (push pop) p. 40
  ;; 31       28                                   15
  ;; [ cond 4 ] [ 100 ] [P] [U] [S] [W] [L] [ Rn ] [ Register list ]
  ((registers  register-list)
   (rn         register)
   (l          load/store)
   (w          write-back)
   (s          psr)
   (u          up/down)
   (p          pre/post)
   (ignored    (forced :value #*001))
   (conditions condition-field)))

(define-binary-class bx (arm-instruction)
  ((rn         register)
   (ignored   (forced :value #*100011111111111101001000))
   (conditions condition-field)))

(define-binary-type b-offset (arm-instruction)
  (:reader (in)
           (let ((buf (make-array 24 :element-type '(unsigned-byte 1))))
             (read-sequence buf in)
             (let* ((signed (not (zerop (bit buf 23))))
                    (base
                     (bits-to-int
                      (concatenate 'simple-bit-vector
                        #*00
                        buf
                        (if signed #*111111 #*000000)))))
               (if signed (- base (expt 2 32)) base))))
  (:writer (out value)
           (let ((buf (int-to-bits
                       (if (negative-integer-p value)
                           (+ (expt 2 32) value)
                           value)
                       32)))
             (write-sequence (subseq buf 2 26) out))))

(define-binary-class b/bl (arm-instruction)
  ((offset     b-offset)
   (l          link-bit)
   (ignored   (forced :value #*101))
   (conditions condition-field)))

(define-binary-class data-processing (arm-instruction)
  ((operand2  (raw-bits :length 12))
   (rd         register)
   (rn         register)
   (s          set-condition-codes)
   (opcode     opcodes)
   (i          immediate)
   (ignored   (forced :value #*00))
   (conditions condition-field)))

(define-binary-class register-operand (arm-data)
  ((rm    register)
   (shift (numerical-value :length 8))))

(define-binary-class immediate-operand (arm-data)
  ((imm    (numerical-value :length 8))
   (rotate (numerical-value :length 4))))

(define-binary-class .word (arm-data)
  ((imm (numerical-value :length 32))))

;;; Interpret data-processing operand2 based on value of immediate operand
(defmethod reconcile-immediate-operand ((obj data-processing))
  (let ((bits (operand2 obj)))
    (format t "BITS:~S~%" bits)
    (setf (operand2 obj)
          (from-bits (make-instance
                         (case (i obj)
                           (:immediate 'immediate-operand)
                           (:register  'register-operand)))
                     bits)))
  obj)

(defmethod (setf i) :after (new (obj data-processing))
  (when (slot-boundp obj 'operand2) (reconcile-immediate-operand obj)))

(defmethod (setf operand2) :after (new (obj data-processing))
  (when (and (slot-boundp obj 'i)
             (subtypep 'SIMPLE-BIT-VECTOR (type-of (operand2 obj))))
    (reconcile-immediate-operand obj)))

(defmethod read-value :around ((type (eql 'data-processing)) stream &key)
  (reconcile-immediate-operand (call-next-method)))

(defmethod write-value :around ((type (eql 'data-processing)) stream value &key)
  (let ((temp (operand2 value)))
    (setf (operand2 value) (to-bits (operand2 value)))
    (call-next-method)
    (setf (operand2 value) temp)))


;;; Convenience methods
(defun make-arm-branch (mnemonic offset)
  (make-instance 'b/bl
    :conditions (ecase mnemonic
                  ((:bl :b) :al)
                  (:bne     :ne)
                  (:beq     :eq))
    :l          (ecase mnemonic
                  (:bl            :branch-w-link)
                  ((:b :bne :beq) :branch))
    :offset offset))

(defmethod set-arm-branch ((obj elf) mnemonic to from)
  (setf (bits-at-ea obj from)
        (to-bits (make-arm-branch mnemonic (- to from 8))))
  obj)

(defun make-arm-data-transfer (mnemonic reg offset)
  (make-instance 'ldr/str
    :conditions :al
    :i :register
    :l (ecase mnemonic (:ldr :load) (:str :store))
    :w :no-write-back
    :b :word
    :u (if (< offset 0) :down :up)
    :p :pre
    :rn 15               ; <- PC
    :rd reg
    :offset (abs offset)))

(defmethod set-arm-data-transfer ((obj elf) mnemonic place reg from)
  (setf (bits-at-ea obj place)
        (to-bits (make-arm-data-transfer mnemonic reg (- from place 8))))
  obj)

(defun make-arm-stack (mnemonic registers)
  (make-instance 'ldm/stm
    :conditions :al
    :p (ecase mnemonic (:push :pre) (:pop :post))
    :u (ecase mnemonic (:push :down) (:pop :up))
    :s :no-psr
    :w :write-back
    :l (ecase mnemonic (:push :store) (:pop :load))
    :rn 13
    :registers (let ((bits (make-array 16 :element-type 'bit
                                       :initial-element 0)))
                 (dolist (reg registers) (setf (bit bits reg) 1))
                 bits)))

(defmethod set-arm-stack ((obj elf) mnemonic place registers)
  (setf (bits-at-ea obj place) (to-bits (make-arm-stack mnemonic registers)))
  obj)

(defun make-arm-data-processing (mnemonic register immediate)
  (make-instance 'data-processing
    :operand2 (make-instance 'immediate-operand
                :rotate 0
                :imm immediate)
    :rd register
    :rn 0
    :s :set
    :opcode mnemonic
    :i :immediate
    :conditions :al))

(defmethod set-arm-data-processing ((obj elf) mnemonic place register immediate)
  (setf (bits-at-ea obj place)
        (to-bits (make-arm-data-processing mnemonic register immediate)))
  obj)

(defun make-arm-word (immediate)
  (make-instance '.word :imm immediate))

(defmethod set-arm-word ((obj elf) place immediate)
  (setf (bits-at-ea obj place) (to-bits (make-arm-word immediate)))
  obj)

(defmethod arm-decode ((obj elf) type ea)
  ;; TODO: shouldn't need a type argument.
  (from-bits (make-instance type) (bits-at-ea obj ea)))
