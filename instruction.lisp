;;; instruction --- comparable machine code instructions
(in-package :elf)

(defclass instruction ()    ; Currently as parsed from Objdump output.
  ((opcode   :initarg :opcode   :accessor opcode)
   (operands :initarg :operands :accessor operands :initform nil)))

(defmethod print-object ((obj instruction) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (cons (opcode obj) (operands obj))
           stream)))

(defgeneric from-string (instruction string)
  (:documentation "Parse an instruction from a string representation."))

(defmethod from-string ((obj instruction) string)
  (unless (zerop (length string))
    (let ((pieces (split-sequence #\Space string :remove-empty-subseqs t))
          (depth 0))
      (setf (opcode obj) (make-keyword (string-upcase (first pieces))))
      (setf (operands obj)
            (when (second pieces)
              (mapcar #'parse-operand
                      ;; Split the instruction encoding STRING into
                      ;; opcode and operands.
                      (split-sequence-if (lambda (char)
                                           (case char
                                             (#\( (incf depth) nil)
                                             (#\) (decf depth) nil)
                                             (#\, (zerop depth))))
                                         (second pieces))))))
    obj))

(defun parse-operand (string)       ; Currently as printed by objdump.
  "Parse an assembly instruction operand."
  ;; FIXME: The quality of this translation is sufficient for
  ;;        comparison and for notional reading, but insufficient for
  ;;        more rigorous semantic applications (e.g., QFBV).
  (flet ((parse-register (string)
           (multiple-value-bind (match matches)
               (scan-to-strings "([abcdesixlp]\+)" (subseq string 1))
             (unless match (error "Failed to parse register ~S" string))
             (make-keyword (string-upcase (aref matches 0))))))
    (case (aref string 0)
      (#\% (parse-register string))                   ; register
      (#\$ (parse-integer string :start 3 :radix 16)) ; literal
      (#\( `(,(parse-register (subseq string 1)))) ; register memory
      (#\* (parse-operand (subseq string 1)))      ; discard leading *
      (#\0                              ; positive offset or literal
       (multiple-value-bind (num index)
           (parse-integer string :start 2 :radix 16 :junk-allowed t)
         (if (= index (length string))
             num
             `(+ (,(parse-register (subseq string (1+ index)))) ,num))))
      (#\-                              ; negative offset or literal
       (multiple-value-bind (num index)
           (parse-integer string :start 3 :radix 16 :junk-allowed t)
         (if (= index (length string))
             num
             `(- (,(parse-register (subseq string (1+ index)))) ,num))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\9 #\8 #\9 ; address
            #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
       (parse-integer string :radix 16 :junk-allowed t))
      (t (error "Unhandled operand ~S" string)))))
