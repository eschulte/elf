;;; instruction --- comparable machine code instructions
(in-package :elf)

(defclass instruction ()
  ((prefixes :initarg :prefixes :accessor prefixes :initform nil)
   (opcode   :initarg :opcode   :accessor opcode)
   (operands :initarg :operands :accessor operands :initform nil)))

(defclass objdump-instruction (instruction) ())

(defclass tsl-instruction (instruction) ())

(defmethod print-object ((obj instruction) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (cons (opcode obj) (operands obj))
           stream)))

(defgeneric from-string (instruction string)
  (:documentation "Parse an instruction from a string representation."))

(defvar instruction-prefixes
  (list "rep" "repe" "repz" "repne" "repnz"
        "data16" "lock"))

(defmethod from-string ((obj objdump-instruction) string)
  (unless (zerop (length string))
    (let ((pieces (split-sequence #\Space string :remove-empty-subseqs t))
          (depth 0))
      (loop :while (member (car pieces) instruction-prefixes :test #'equal)
         :do (push (pop pieces) (prefixes obj)))
      ;; TODO: handle data declarations e.g.,
      ;;     400645:       66 66 2e 0f 1f 84 00 data32 nopw %cs:0x0(%rax,%rax,1)
      (setf (opcode obj) (make-keyword (string-upcase (first pieces))))
      (setf (operands obj)
            (when (and (second pieces)
                       (not (member (opcode obj)
                                    (list :data32))))
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
               (scan-to-strings
                 "(xm{2}[0-9][0-5]\?|[abcdesixlp]\+|r[1-9][0-5]\?)"
                 (subseq string 1))
             (unless match (error "Failed to parse register ~S" string))
             (make-keyword (string-upcase (aref matches 0))))))
    (case (aref string 0)
      (#\% (parse-register string))                   ; register
      (#\$ (parse-integer string :start 3 :radix 16)) ; literal
      (#\( `(,(parse-register (subseq string 1)))) ; register memory
      (#\* (parse-operand (subseq string 1)))      ; discard leading *
      (#\0                              ; positive offset or literal
       (if (string= string "0")
           0
           (multiple-value-bind (num index)
               (parse-integer string :start 2 :radix 16 :junk-allowed t)
             (if (= index (length string))
                 num
                 `(+ (,(parse-register (subseq string (1+ index)))) ,num)))))
      (#\-                              ; negative offset or literal
       (multiple-value-bind (num index)
           (parse-integer string :start 3 :radix 16 :junk-allowed t)
         (if (= index (length string))
             num
             `(- (,(parse-register (subseq string (1+ index)))) ,num))))
      ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 ; address
            #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
       (parse-integer string :radix 16 :junk-allowed t))
      (t (error "Unhandled operand ~S" string)))))

(defmethod from-string ((obj tsl-instruction) string)
  (unless (zerop (length string))
    ;; Important part is in the parens (last parent is always last character).
    (let* ((index (let ((index 0))
                    (loop :do (incf index)
                       :until (or (> index (length string))
                                  (equal (aref string index) #\()))
                    index))
           (ops (split-sequence
                    #\, (subseq string (1+ index) (1- (length string))))))
      (setf (opcode obj) (parse-tsl-operand (nth 3 ops)))
      (setf (operands obj) (mapcar #'parse-tsl-operand (subseq ops 4)))
      obj)))

(defmethod parse-tsl-operand (string)
  (when (equal (aref string 0) #\Space) (setf string (subseq string 1)))
  (flet ((beginning (substring)
           (and (>= (length string) (length substring))
                (string= (subseq string 0 (length substring)) substring))))
    (cond
      ((beginning "MOV") :mov)
      ((beginning "EAX") :eax)
      ((beginning "RegDirect32") (list (parse-tsl-operand
                                        (subseq string
                                                (1+ (length "RegDirect32"))
                                                (1- (length string))))))
      ((beginning "Immediate32") (parse-integer
                                  (subseq string
                                          (1+ (length "RegDirect32"))
                                          (1- (length string)))))
      (t string))))
