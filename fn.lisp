;;;; fn.lisp -- defn and fn facilities for the fn programming language

(in-package :fn-impl)


(defun arg-list-components (l)
  "Returns a list [POS OPT KEY REST], where POS is the positional arguments of
the arg list, OPT is the optional arguments [[name initform] ... ], KEY is the
keyword arguments [[:key sym initform] ... ], and REST is the name of a rest
variable or NIL."
  (let (pos opt key rest)
    ;; FIXME: this would probably be better expressed with LOOP
    (labels ((iter (tl pos-p)           ;process the argument list
               ;; this is probably a mistake and can yield some whacky behavior
               (when (member (car tl) '(&rest &whole &body &key &optional))
                 (warn "FN lambda lists don't understand ~a arguments~%"
                       (car tl)))
               (cond
                 ((not tl) nil)
                 ((eq (car tl) '&)      ;varargs
                  (if (= (length tl) 2) ;must be the last element
                      (setq rest (cadr tl))
                      (error "Malformed FN llambda list")))
                 ((and (symbolp (car tl)) pos-p) ;positional argument
                  (push (car tl) pos)
                  (iter (cdr tl) pos-p))
                 ((and (listp (car tl)) (keywordp (caar tl))) ;key argument
                  (push (car tl) key)
                  (iter (cdr tl) nil))
                 ((and (listp (car tl)) (symbolp (caar tl))) ;optional arg
                  (push (car tl) opt)
                  (iter (cdr tl) nil))
                 (t (error "Malformed FN lambda list"))))
             (fix-key (keyform)         ;make key arguments suit CL
               (let ((name (intern (symbol-name (car keyform)))))
                 (cons name (cdr keyform)))))
      (iter l t)
      (nconc (mapcar #'nreverse [pos opt key])
             [rest]))))
 

(defun convert-arg-list (l)
  "Convert an FN-style arg-list to a Common Lisp one"
  (destructuring-bind (pos opt key rest)
      (arg-list-components l)
    (labels ((fix-key (keyform)         ;make key arguments suit CL
               (let ((name (intern (symbol-name (car keyform)))))
                 (cons name (cdr keyform)))))
      ;; build the ordinary lambda list
      (append pos
              (if opt '(&optional))
              opt
              (if key '(&key))
              (mapcar #'fix-key key)
              (if rest
                  ['&rest rest])))))

(defun arg-list-vars (l)
  "Get the variable bound by an FN-style args list"
  (destructuring-bind (pos opt key rest)
      (arg-list-components l)
    (nconc pos
           (mapcar #'car opt)
           (mapcar $(intern (symbol-name (car $))) key)
           (if rest [rest] []))))

(defun lambda-list-vars (l)
  "Get symbols that are bound by the ordinary CL lambda list"
  (->> l
    (remove-if $(and (symbolp $)
                     (equal (aref (symbol-name $) 0) #\&)))
    (mapcar $(if (symbolp $)
                 $
                 (if (symbolp (car $))
                     (car $)            ; name for optional or key
                     (cadar $))))))     ; cadr of a (:key sym) pair


;;; supported options:
;; - :curry (INT or BOOL)
;; - :memoize (INT or BOOL)
;; - :doc STR
;; - :argdoc STR
(defparameter fn-default-options (dict :curry nil
                                       :memoize nil
                                       :doc ""
                                       :argdoc ()))

(defun has-options (fn-body)
  "Returns true when fn-body starts with an options dict."
  (and (>= (length fn-body) 2)
       (listp (car fn-body))
       (eq (caar fn-body) 'dict)))

(defun get-options (fn-body)
  "Get the options from an fn-body by merging the fn-body arguments with the
 default fn options."
  (if (has-options fn-body)
      (let ((opts (apply #'dict (cdar fn-body))))
        ;; argument order is important here
        (dict-extend fn-default-options opts))
      fn-default-options))

;; (defun fn-declarations (opts)
;;   "Generate a list of declare forms based on the function options."
;;   ;; TODO
;;   ())

(defun memoize (f)
  "Create a memoized version of f."
  (let ((memo (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val bound) (gethash args memo)
        (if bound
            val
            (progn
              (let ((val0 (apply f args)))
                (setf (gethash args memo) val0)
                val0)))))))


(defun curry-transform (curry-times lambda-form)
  "Wrap a lambda form to automatically curry the first several arguments."
  (declare (type integer curry-times))
  (let ((a* (gensym))
        (b* (gensym)))
    `(lambda (&rest ,a*)
       (if (<= (length ,a*) ,curry-times)
           (lambda (&rest ,b*)
             (apply ,lambda-form (append ,a* ,b*)))
           (apply ,lambda-form ,a*)))))


(defmacro fn (arg-list &body body)
  "Like lambda, but with different argument list and support for options"
  (let* ((lform `(lambda ,(convert-arg-list arg-list) . ,body))
         (lopt (get-options body)))
    (let ((fun (gensym)))
      ;; we have to wrap the function with memoize before currying it if we're
      ;; doing both
      `(let ((,fun ,(aif (dict-get lopt :memoize)
                         `(memoize ,lform)
                         lform)))
         ,(aif (dict-get lopt :curry)
               (curry-transform it fun)
               fun)))))

;; ;; test code
;; (setf (symbol-function '+c) (fn (a & a*)
;;                               {:curry 1}
;;                               (apply #'+ a a*)))
;; (setf (symbol-function '+slow) (fn (a & a*)
;;                                  {:curry 1 :memoize t}
;;                                  (sleep 2.0)
;;                                  (apply #'+ a a*)))

