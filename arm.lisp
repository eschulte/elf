;;; arm.lisp --- Support for reading and writing ARM instructions

;;; Commentary.
;;
;; This is implemented largely from the ARM7-TDMI-manual-pt2.pdf and
;; page numbers refer to said.

;;; Code.
(in-package :elf)

(defclass arm-instruction () ())


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

(define-bit-dictionary psr 1
  ((0 . :no-psr) (1 . :psr)))

(define-binary-type register ()
  (unsigned-integer :bytes 1 :byte-size 1))

(define-binary-type register-list () (raw-bits 16))


;;; ARM Instructions
(define-binary-class ldr/str (arm-instruction)
  ;; LDR/STR opcodes p. 28
  ;; 31       28                             19      15      11          0
  ;; [ Cond 4 ] [01] [I] [P] [U] [B] [W] [L] [ Rn 4] [ Rd 4] [ Offset 12 ]
  ((offset    (raw-bits 12))
   (rd         register)
   (rn         register)
   (l          load/store)
   (w          write-back)
   (b          byte/word)
   (u          up/down)
   (p          pre/post)
   (i          immediate)
   (ignored   (raw-bits :length 2))     ; should equal #*01
   (conditions condition-field)))

(define-binary-class ldm/stm ()
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
   (ignored    (raw-bits :length 3))    ; should equal #*100
   (conditions condition-field)))


;;; Convenience methods
;;
;; TODO: update to use the above
;; 
(defmethod arm-jump ((obj elf::elf) type to from)
  (let ((op-code
         (ecase type
           (:bl  (if (> to from) '(0   235) '(255 235)))
           (:b   (if (> to from) '(0   234) '(255 234)))
           (:bne (if (> to from) '(0   26)  '(255 26)))
           (:beq (if (> to from) '(0   10)  '(255 10)))))
        (operand
         (let ((distance (/ (- to from 8) 4)))
           (when (< to from) (incf distance (expt 2 16)))
           (int-to-bytes distance 2))))
    (setf (word-at-ea obj from)
          (concatenate 'vector operand op-code)))
  obj)

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
               (aget :condition  disasm)
               (if (eql *endian* :little) #*10 #*01)
               (aget :immediate  disasm)
               (aget :pre/post   disasm)
               (aget :up/down    disasm)
               (aget :byte/word  disasm)
               (aget :write-back disasm)
               (aget :load/store disasm)
               (aget :base-reg   disasm)
               (aget :dest-reg   disasm)
               (aget :offset     disasm))))
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
