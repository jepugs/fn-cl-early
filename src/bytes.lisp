(defpackage :fn.bytes
  (:use :cl :fn.util :fn.ast))

(in-package :fn.bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Instructions

(deftype uint (i) `(unsigned-byte ,i))
(deftype int (i) `(signed-byte ,i))
(deftype instruction () '(unsigned-byte 32))
(deftype chunk () '(vector (unsigned-byte 32)))

(declaim (inline get-opcode
                 get-field-a
                 get-field-b
                 get-field-c
                 get-field-bx
                 make-instr
                 make-instr2))

(defun get-opcode (bc)
  (-> bc
    (ash -24)
    (coerce '(uint 8))))

(defun get-field-a (bc)
  (declare (type instruction bc))
  (-> bc
    (logand #x00ff0000)
    (ash -16)
    (coerce '(uint 8))))

(defun get-field-b (bc)
  (declare (type instruction bc))
  (-> bc
    (logand #x0000ff00)
    (ash -8)
    (coerce '(uint 8))))

(defun get-field-c (bc)
  (declare (type instruction bc))
  (-> bc
    (logand #x000000ff)
    (coerce '(uint 8))))

(defun get-field-bx (bc)
  (declare (type instruction bc))
  (-> bc
    (logand #x0000ffff)
    (coerce '(uint 16))))

(defun get-field-sbx (bc)
  (declare (type instruction bc))
  (-> bc
    (logand #x0000ffff)
    (coerce '(int 16))))

(defun make-instr (op a b c)
  (declare (type (uint 8) op a b c))
  (coerce (logior (ash op 24)
                  (ash a 16)
                  (ash b 8)
                  c)
          'instruction))

(defun make-instr2 (op a bx)
  (declare (type (uint 8) op a)
           (type (uint 16) k))
  (coerce (logior (ash op 24)
                  (ash a 16)
                  bx)
          'instruction))


;; lst.hd
;; lst.tl
;; ;; these replace
;; (car lst)
;; (cdr lst)


;;; Notation

;;; - A,B,C are 1-byte constants encoded in the instruction, (often corres. to registers)
;;; - Bx is a 2-byte constant used by some instructions instead of B and C
;;; - R(x) is value of the register x (1-byte constant)
;;; - *R(x) is the memory location pointed at by R(x)
;;; - V(x) loads a global variable using the symbol ID in xx
;;; - K(xx) is the constant value with ID xx (2-byte constant)

;;; - 

(defparameter opcodes
  '(;; loading and storing
    op-cp    #xC0    ; (CP A B)     copy R(A) := R(B)
    op-ldconst #xC1  ; (LDK A Bx)   load constant R(A) := K(Bx)
    op-ldvar #xC2    ; (LDV A)      loads a global variable R(A) := V(B)
    op-st    #xC4    ; (ST A B)     store *R(A) := R(B)
    op-stv   #xC5    ; (STV A B C)  store V(A) := R(B). C has additional flags

    ;; tests
    ;; tests skip the next instruction on success & are always followed by JMP
    op-eq    #xD0    ; (EQ A B C)
    ;; floating point tests (these don't check types)
    op-gt    #xD1    ; (GT A B C)
    op-lt    #xD2    ; (LT A B C)
    op-geq   #xD3    ; (GEQ A B C)
    op-leq   #xD4    ; (LEQ A B C)

    ;; control flow and function calls
    op-jmp   #xC6    ; (JMP A)       unconditional jump to PC + R(A)
    op-call  #xC9    ; (CALL A B C) call the function in R(A). See docs for more
    op-ret   #xCA    ; (RET A B)    return from a function

    ;; maths
    op-add   #xCB    ; (ADD A B C)  R(A) := R(B) + R(C)
    op-sub   #xCC    ; (SUB A B C)  R(A) := R(B) - R(C)
    op-mul   #xCD    ; (MUL A B C)  R(A) := R(B) * R(C)
    op-div   #xCE    ; (DIV A B C)  R(A) := R(B) / R(C)

    ;; lisp
    op-ldempty #xFF  ; (LDEMPTY A)  R(A) := empty list constant
    op-cons  #xFF    ; (CONS A B C) R(A) := new cons B C. Emits an error if C is not a list
    op-ldtrue #x00   ;
    op-ldfalse #x00  ;
    op-ldsym   #x00  ; (LDS A Bx)   load symbol R(A) := S(Bx)


    op-def    #x00  ; (DEF A B C) define V(B); R(A) := V(A). C is flags
    ;; fetch a value from an object
    op-new    #x00  ; (NEW A B C)
    op-addkey #x00  ; (ADDKEY A B C)
    op-setkey #x00  ; (SETKEY A B C)

    op-close   #x00  ; adds register A to the 
    ))

(defmacro bytecode (&body commands)
  ;; SBCL lets us just return an array, so let's just shunt as much processing as possible to the
  ;; macroexpansion phase.
  (make-array (length commands)
              :element-type 'instruction
              :initial-contents
              (mapcar $(let ((op-name (symb "OP-" (car $))))
                         (aif (getf opcodes op-name)
                              (cond
                                ((length= $ 2) (make-instr2 it (cadr $) 0))
                                ((length= $ 3) (make-instr2 it (cadr $) (caddr $)))
                                ((length= $ 4) (make-instr it (cadr $) (caddr $) (cadddr $)))
                                (error "BYTECODE macro error: wrong number of args ~a" $))
                              (error "BYTECODE macro error: unknown operation ~a" $)))
                      commands)
              :adjustable nil))

(defun decompile-chunk (chunk)
  (declare (type chunk chunk))
  (let ((ops-rev (reverse opcodes)))
    (map 'list
         $(list (getf ops-rev (get-opcode $)))
         chunk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; VM data structures

(deftype val ())

(defclass val ()
  ((tag :initarg :tag
        :type (uint 32))
   (data :initarg :data))
  (:documentation "VM representation of an FN object. Eventually we will fix the type of data to 8
 bytes, but this is good enough for us for now."))

(defclass obj ()
  (type table))

(defclass symb ()
  ((id :initarg :id
       :type (uint 16))
   (name :initarg :name
         :type string)))

(defvar empty-symb
  (make-instance 'symb :id 0 :name ""))

(defclass vm ()
  ((symb-name-tab :initarg :symb-name-tab
                  :type hash-table
                  :initform (make-hash-table)
                  :documentation "Symbol table indexed by name (string)")
   (symb-tab :initarg :symb-tab
             :type (vector symb)
             :initform (make-array 256 :fill-pointer 0)
             :documentation "Symbol table indexed by symbol ID")
   (next-symb-id)
   (var-tab :initarg :var-tab
            :type hash-table
            :initform (make-hash-table)
            :documentation "Variable table indexed by symbol ID")
   (call-stack :initarg :call-stack
               :type list
               :initform nil)
   (closed-regs :type list
                :initform nil
                :documentation "Registers which must be copied to a closure on return")
   (regs :initarg :regs
         :type simple-vector
         :initform (make-array 256)
         :documentation "Array of values constituting the machine's registers.")
   (pc :initarg :pc
       :type (uint 32)
       :initform 0)
   (code :initarg :code
;;         :type chunk
         ))
  (:documentation "VM state"))

(defclass call-frame ()
  ((num-args)
   (var-args)
   (inactive-regs)
   (return-addr)))


(defun float-bits (f)
  (declare (optimize (safety 0)
                     (speed 3)))
  (the (uint 64) f))
