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
(defmethod set-arm-branch ((obj elf) mnemonic to from)
  (setf (bits-at-ea obj from)
        (to-bits (make-instance 'b/bl
                   :conditions (ecase mnemonic
                                 ((:bl :b) :al)
                                 (:bne     :ne)
                                 (:beq     :eq))
                   :l          (ecase mnemonic
                                 (:bl            :branch-w-link)
                                 ((:b :bne :beq) :branch))
                   :offset (- (- from 8) to))))
  obj)

(defmethod set-arm-data-transfer ((obj elf) mnemonic place reg from)
  (setf (bits-at-ea obj from)
        (to-bits (make-instance 'ldr/str
                   :conditions :al
                   :i :immediate
                   :l (ecase mnemonic (:ldr :load) (:str :store))
                   :w :no-write-back
                   :b :word
                   :u (if (>= from (+ place 8)) :up :down)
                   :p :pre
                   :i :immediate
                   :rn 15               ; <- PC
                   :rd reg
                   :offset (- from place 8))))
  obj)

(defmethod arm-decode ((obj elf) type ea)
  ;; TODO: shouldn't need a type argument.
  (from-bits (make-instance type) (bits-at-ea obj ea)))
