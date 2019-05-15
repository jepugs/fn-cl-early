;;;; params.lisp -- operations on parameter and argument lists

;;;; This file is part of fn.

;;;; fn is free software: you can redistribute it and/or modify it under the terms of the GNU
;;;; General Public License as published by the Free Software Foundation, either version 3 of the
;;;; License, or (at your option) any later version.

;;;; fn is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
;;;; the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License along with fn. If not, see
;;;; <https://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :fn-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - MAKE-ARGS :: create an argument list
;; - MAKE-PARAMS :: create a PARAMS object from a PARAM-LIST
;; - BIND-ARGS :: create a bindings hash table from a

;;; A _parameter_ is of the form NAME, or (NAME DEFAULT-VALUE). _Parameter lists_ are lists of
;;; parameters which may optionally have the symbol '& as the second-to-last element, (which
;;; indicates that variable number of arguments may be accepted by the function and will be passed
;;; to the parameter .


(defclass pparams ()
  ((positional :initarg :positional
               :type list
               :documentation "Positional argument, including optional ones. Each element in the
                              list is either a symbol or a list (VAR-NAME DEFAULT-INITFORM) in the
                              case of optional arguments. Optional arguments must come after
                              positional arguments.")
   (keyword :initarg :keyword
            :type list
            :documentation "Keyword arguments. These are lists of the form (KEYWORD VAR-NAME)
                           or (KEYWORD (VAR-NAME DEFAULT-FORM)).")
   (rest :initarg :rest
         :type symbol
         :documentation "Rest parameter name or NIL."))

  (:documentation "\"Parsed parameters.\" An intermediate representation of a parameter list."))

(defun namep (p)
  "Tell if P is a valid param list variable name."
  (and (not (keywordp p))
       (symbolp p)))

(defun optp (p)
  "Tell if P is a valid optional argument form."
  (and (listp p)
       (= (length p) 2)
       (namep (car p))))

(defun par-name (p)
  "Get the variable name of a parameter."
  (if (listp p)
      (if (keywordp (car p))
          (cadr p)
          (car p))
      p))

;; FIXME: add checks for illegal or duplicate variable names
(defun make-pparams (param-list)
  "Create a new PPARAMS object from a parameter list.

  Parameters
  ----------
  PARAM-LIST : list
               An fn parameter list.

  Returns
  -------
  P : PPARAMS
      The PPARAMS object corresponding to PARAM-LIST."
  (labels
      ;; These functions work together to create an argument list for (MAKE-INSTANCE 'PPARAMS).
      ;; After collecting positional arguments, GET-POSITIONAL calls GET-KEYWORD or GET-REST.
      ;; GET-KEYWORD collects GET-REST arguments and
      ((parse-params (lst)
         (get-positional nil lst))
       (get-positional (acc lst &optional (optional nil))
         (cond
           ((null lst)
            (list :positional (nreverse acc) :keyword nil :rest nil))
           ((keywordp (car lst))
            `(:positional ,(nreverse acc) ,@(get-keyword nil lst)))
           ((eq (car lst) '&)
            `(:positional ,(nreverse acc) :keyword nil ,@(get-rest lst)))
           ((optp (car lst))
            (get-positional (cons (car lst) acc) (cdr lst) t))
           ((namep (car lst))
            ;; it's illegal to have non-optional positional arguments after optional ones.
            (if optional
                (error "MAKE-PPARAMS: Non-optional positional arguments cannot succeed optional ones.")
                (get-positional (cons (car lst) acc) (cdr lst))))
           (t (error "MAKE-PPARAMS: Illegal object in param-list: ~s" (car lst)))))
       (get-keyword (acc lst)
         (cond 
           ((null lst)
            (list :keyword (nreverse acc) :rest nil))
           ((keywordp (car lst))
            (when (null (cdr lst))
              (error
               "MAKE-PPARAMS: Missing keyword variable in ~s"
               param-list))
            (let ((k (car lst))
                  (v (cadr lst)))
              (unless (and (keywordp k)
                           (or (namep v)
                               (optp v)))
                (error "MAKE-PPARAMS: Malformed keyword pair in ~s" param-list))
              (get-keyword (cons (list k v) acc)
                           (cddr lst))))
           ((eq (car lst) '&)
            `(:keyword ,(nreverse acc) ,@(get-rest lst)))
           (t (error "MAKE-PPARAMS: Invalid form in keyword part of ~s" param-list))))
       (get-rest (lst)
         (unless (and (= (length lst) 2)
                      (namep (cadr lst)))
           (error "MAKE-PPARAMS: Malformed rest part in ~s" param-list))
         (list :rest (cadr lst))))
    (apply #'make-instance 'pparams (parse-params param-list))))


(defun pparams-has-rest (p)
  "Tell if a PPARAMS object has a rest parameter.

  Parameters
  ----------
  P : PPARAMS

  Returns
  -------
  X : boolean

"
  (if (slot-value p 'rest)
      t
      nil))



(defun pos-args-plist (pos-params args)
  "Helper for BIND-PARAMS. Creates a plist of positional bindings."
  (loop for p in pos-params
     for i = 0 then (1+ i)
     for as = args then (cdr as)
     for name = (if (listp p) (car p) p)
     append (cond
              ((eq name '_) nil)        ;wildcard (ignore)
              (as (list name (car as))) ;value provided
              ((listp p) p)             ;optional argument
              (t (error "BIND-PARAMS: Missing required positional argument ~s" i)))))

(defun key-args-plist (keyword rest args)
  "Helper for BIND-PARAMS. Creates a plist of keyword bindings."
  (let ((ks (mapcar #'car keyword))
        (ht (apply #'make-ht args)))
    (unless (every #'keywordp (ht-keys ht))
      (error "BIND-PARAMS: Non-keyword in key part of arguments"))
    (let ((non-rest (mapcan $(if (eq (cadr $) '_)
                                 nil    ;wildcard
                                 (multiple-value-bind (val exists)
                                     (gethash (car $) ht)
                                   (if exists
                                       (if (listp (cadr $))
                                           (list (caadr $) val)
                                           (list (cadr $) val))
                                       (if (listp (cadr $))
                                           (cadr $)
                                           (error "BIND-PARAMS: Missing required key argument ~s"
                                                  (car $))))))
                            keyword))
          (rest-list (ht->plist (ht-del-keys ht ks))))
      (if rest
          `(,rest ,rest-list . ,non-rest)
          (if rest-list
              (error "BIND-PARAMS: Extraneous keywords ~s" rest-list)
              non-rest)))))

(defun rest-args-plist (rest args)
  "Helper for BIND-PARAMS. Create a plist of rest args."
  (if rest
      (list rest args)))

(defun bind-params (params arg-list)
  "Given a parameter list and a list of values, returns a hash table mapping parameters to the
  values in ARG-LIST. This is the inverse operation of BACK-SUB-PARAMS, i.e. the expression

  (BIND-PARAMS P (BACK-SUB-PARAMS P BINDINGS))

  should return a new hash table with the same contents as BINDINGS.

  Parameters
  ---------- 
  PARAMS : PPARAMS or list
           Parameter list to use.

  ARG-LIST : List of arguments to do destructuring on.

  Returns
  -------
  A hash table that binds every variable in PARAMS to a value in ARG-LIST.

"
  (if (typep params 'pparams)
      (with-slots (positional keyword rest) params
        (let ((pos (take (length positional) arg-list))
              (key-or-rest (drop (length positional) arg-list)))
          (apply #'make-ht
                 (append (pos-args-plist positional pos)
                         (cond
                           (keyword (key-args-plist keyword rest key-or-rest))
                           (rest (rest-args-plist rest key-or-rest))
                           (key-or-rest (error "BIND-ARGS: Illegal trailing arguments."))
                           (t nil))))))
      (bind-params (make-pparams params) arg-list)))


(defun back-sub-params (params bindings)
  "Given a parameter list or a and a hash table that maps parameters to some values, generate an
  argument list which would have resulted in these bindings. This is the inverse operation of
  BIND-PARAMS, i.e. the expression

  (BACK-SUB-PARAMS P (BIND-PARAMS P ARGS))

  should return a new list with the same contents as ARGS.

  Parameters
  ----------
  PARAMS : PPARAMS or list
           Parameter list to use

  BINDINGS: A hash table that binds every variable in PARAMS to a value

  Returns
  -------
  A list of arguments that would result in the provided bindings when destructured with PARAMS.

"
  (if (typep params 'pparams)
      (with-slots (positional keyword rest) params
        (let* ((names (append (mapcar (lambda (x)
                                       (if (listp x)
                                           (car x)
                                           x))
                                      positional)
                              (mapcar (lambda (x)
                                        (if (listp (cadr x))
                                            (caadr x)
                                            (cadr x)))
                                      keyword)))
               (fixargs (mapcar (lambda (x)
                                  ;; packageless symbols starting with WILD-GENSYM-PREFIX are
                                  ;; treated as wildcards
                                  (if (wild-gensym-p x)
                                      '_
                                      (gethash x bindings)))
                                names)))
          (if (and rest
                   (not (wild-gensym-p rest)))
              (append fixargs (gethash rest bindings))
              fixargs)))
      (back-sub-params (make-pparams params) bindings)))


(defun params->lambda-list (params)
  "Given fn params, generate an equivalent Common Lisp lambda list. Common Lisp lambda lists provide
  all the functionality needed by fn parameter lists.

  The one caveat here comes from the wildcard symbol, '_. This symbol is replaced with a (GENSYM) in
  the resultant lambda list, with the understanding that a (DECLARE (IGNORE ...)) form will be added
  to the body of the any generated LAMBDA forms.

  Parameters
  ----------
  PARAMS : either a parameter list or a PPARAMS object

  Returns
  -------
  LL : a regular Common Lisp lambda list

"
  (if (typep params 'pparams)
      (with-slots (positional keyword rest) params
        (let* ((key-err '(error "Missing required keyword argument"))
               (req-and-opt (split-when #'listp positional))
               (args0 (append (car req-and-opt)
                              (if (cadr req-and-opt)
                                  (cons 'cl:&optional (cadr req-and-opt))
                                  nil)
                              (if keyword
                                  (cons 'cl:&key
                                        (mapcar (lambda (x)
                                                  (if (listp (cadr x))
                                                      `((,(car x) ,(caadr x)) ,(cadadr x))
                                                      `((,(car x) ,(cadr x)) ,key-err)))
                                                keyword)))
                              (if rest
                                  (if keyword
                                      (list 'cl:&allow-other-keys rest)
                                      (list 'cl:&rest rest))
                                  nil))))
          ;; replace wildcards with GENSYMs
          (mapcar (lambda (x)
                    (if (eq x '_)
                        (gensym wild-gensym-prefix)
                        x))
                  args0)))
      (params->lambda-list (make-pparams params))))
