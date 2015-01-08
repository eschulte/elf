;;; arm.lisp --- Support for reading and writing ARM instructions

;;; Commentary.
;;
;; This is implemented largely from the ARM7-TDMI-manual-pt2.pdf and
;; page numbers refer to said.

;;; Code.
(in-package :elf)

(defclass arm-instruction () ())

(defmethod initialize-instance :after ((obj arm-instruction) &key)
  (setf (ignored obj) nil))

(defmethod from-bits ((obj arm-instruction) bits)
  (setf obj (read-value (type-of obj) (make-in-memory-input-stream bits))))

(defmethod to-bits ((obj arm-instruction))
  (with-output-to-sequence (out :element-type '(unsigned-byte 1))
    (write-value (type-of obj) out obj)))


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
  ((0 . :immediate) (1 . :register)))

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

(define-binary-type offset (length)
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
  ((offset    (offset :length 12))
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


;;; Convenience methods
(defmethod arm-jump ((obj elf::elf) mnemonic to from)
  (make-instance 'b/bl
    :conditions (ecase mnemonic
                  ((:bl :b) :al)
                  (:bne     :ne)
                  (:beq     :eq))
    :l          (ecase mnemonic
                  (:bl            :branch-w-link)
                  ((:b :bne :beq) :branch))
    :offset (- (- from 8) to))
  ;; (setf (word-at-ea obj from)
  ;;       (concatenate 'vector operand op-code))
  ;; obj
  )

(defmethod arm-bl ((obj elf::elf) to from)
  (arm-jump obj :bl to from))

(defmethod arm-b ((obj elf::elf) to from)
  (arm-jump obj :b to from))

(defmethod arm-bne ((obj elf::elf) to from)
  (arm-jump obj :bne to from))

(defmethod arm-beq ((obj elf::elf) to from)
  (arm-jump obj :beq to from))

(defun arm-disasm (bits)
  (flet ((bseq (from to)
           (if (eq *endian* :little)
               (subseq bits from to)
               (subseq bits (- 32 from) (- 32 to)))))
    (assert (equalp (bseq 26 28) (if (eq *endian* :little) #*10 #*01))
            (bits)
            "does not look like a valid LDR/SDR instruction ~a"
            (bseq 26 28))
    `((:condition  . ,(bseq 28 32))
      (:immediate  . ,(bseq 25 26))
      (:pre/post   . ,(bseq 24 25))
      (:up/down    . ,(bseq 23 24))
      (:byte/word  . ,(bseq 22 23))
      (:write-back . ,(bseq 21 22))
      (:load/store . ,(bseq 20 21))
      (:base-reg   . ,(bseq 16 20))
      (:dest-reg   . ,(bseq 12 16))
      (:offset     . ,(bseq 0  12)))))

(defun arm-dosasm (disasm)
  (let ((bits (list
               (cdr (assoc :condition  disasm))
               (if (eql *endian* :little) #*10 #*01)
               (cdr (assoc :immediate  disasm))
               (cdr (assoc :pre/post   disasm))
               (cdr (assoc :up/down    disasm))
               (cdr (assoc :byte/word  disasm))
               (cdr (assoc :write-back disasm))
               (cdr (assoc :load/store disasm))
               (cdr (assoc :base-reg   disasm))
               (cdr (assoc :dest-reg   disasm))
               (cdr (assoc :offset     disasm)))))
    (apply #'concatenate '(vector (unsigned-byte 1))
           (if (eq *endian* :little) (reverse bits) bits))))

(defmethod arm-ldr ((obj elf::elf) place register from)
  "Edit ldr instruction at PLACE to load FROM into REGISTER"
  (setf (word-at-ea obj place)
        (int-to-bytes
         (bits-to-int
          (arm-dosasm
           `((:condition  . #*0111)
             (:immediate  . #*0)
             (:pre/post   . #*1)
             (:up/down    . ,(if (> from place) #*1 #*0))
             (:byte/word  . #*0)
             (:write-back . #*0)
             (:load/store . #*1)
             (:base-reg   . ,(int-to-bits 15 4)) ; PC base register
             (:dest-reg   . ,(int-to-bits register 4))
             (:offset     . ,(int-to-bits (abs (- (- from place) 8)) 12))))) 4))
  obj)
