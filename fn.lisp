;;;; fn.lisp -- arg-list processing and the fn lambda replacement macro

(in-package :fn-impl)

;;;;;;
;;; Args objects (argument processing code)
;;;;;;

(defclass args-object ()
  ((pos :initarg :pos
        :type list
        :documentation "Positional arguments as symbols")
   (opt :initarg :opt
        :type list
        :documentation "Optional arguments in the form (var default)")
   (kw :initarg :kw
       :type list
       :documentation "Keyword arguments in the form (:KW var default)")
   (rest :initarg :rest
         :documentation "Rest parameter (if named)")
   (has-kw :initarg :has-kw
           :type boolean
           :documentation
           "Whether this args object has keywords (mutually exclusive with
 use-rest)")
   (has-rest :initarg :has-rest
             :type boolean
             :documentation
             "Whether this args object has a rest arg (mutually exclusive with
 use-keys)"))

  (:documentation "An object built from an fn arg-list which is structured for
 efficient list destructuring"))

(defun is-name (x)
  "Tell if x is a name-- a non-keyword, non-null, non-T, non-& symbol."
  (and (symbolp x)
       x
       (not (eq x t))
       (not (eq x '&))
       (not (keywordp x))))

(defun is-pos-arg (x)
  "Tell if x is a positional argument (has form (NAME INITFORM))."
  (is-name x))

(defun is-opt-arg (x)
  "Tell if x is an optional argument (has form (NAME INITFORM))."
  (and (listp x)
       (is-name (car x))
       (= (length x) 2)))

(defun check-key (key arg)
  "Check whether key and arg form a valid keyword arg pair."
  (and (keywordp key)
       (or (is-pos-arg arg)
           (is-opt-arg arg))))

(defun make-args (l)
  "Make an args object from an args-list l"
  (mapc
   $(when (member $ '(&rest &whole &body &key &optional))
      ;; the user /probably/ made a mistake; this causes whacky behavior
      (warn "FN lambda lists don't understand ~a arguments~%" $))
   l)
  (let* ((pos-and (split-if (complement #'is-pos-arg) l))
         (opt-and (split-if (complement #'is-opt-arg) (cadr pos-and)))
         (pos (car pos-and))            ;positional args
         (opt (car opt-and))            ;optional args
         (other (cadr opt-and)))
    (if (eq (car other) '&)             ;check for & or keywords
        ;; & argument-- ensure exactly one name remains after &
        (cond ((not (is-name (cadr other)))
               (error "make-args: illegal name for arg after &"))
              ((not (= (length other) 2))
               (error "make-args: extra args after &"))
              (t (make-instance 'args-object
                                :pos pos
                                :opt opt
                                :kw []
                                :rest (cadr other)
                                :has-kw nil
                                :has-rest t)))
        ;; on to the keywords (or nothing)
        (let ((kw* (group 2 other)))
          ;; check keyword syntax
          (mapc $(unless (check-key (car $) (cadr $))
                   (error "make-args: malformed keyword argument ~s" $))
                kw*)
          (make-instance 'args-object
                         :pos pos
                         :opt opt
                         ;; flatten optional args
                         :kw kw*
                         :rest nil
                         :has-kw (and kw* t)
                         :has-rest nil)))))

(defun convert-arg-list (l)
  "Convert an FN-style arg-list to a Common Lisp one"
  (with-slots (pos opt kw rest has-kw has-rest) (make-args l)
    (labels ((fix-key (k)         ;make key arguments suit CL
               (if (listp (cadr k))
                   ;; with default value
                   `((,(car k) ,(caadr k)) ,(cadadr k))
                   `((,(car k) ,(cadr k))
                     (error "Missing required keyword argument ~s" ,(car k))))))
      ;; build the ordinary lambda list
      (append pos
              (if opt '(&optional) [])
              opt
              (if has-kw '(&key) []) 
              (mapcar #'fix-key kw)
              (if has-rest
                  ['&rest rest])))))

(defun arg-obj-vars (a)
  "Get the variable bound by an args-object"
  (with-slots (pos opt kw rest) a
    (nconc pos
           (mapcar #'car opt)
           (mapcar #'cadr kw)
           (if rest [rest] []))))

(defun destruct-pos-opt (pos-opt l)
  "Helper function that matches positional and optional argument names to values
 in l. Emits an error if there are not enough elements in l. pos-opt is a
 concatenated list of positional and optional argument forms. Returns a
 dictionary of the mapping and all arguments after matching."
  (rlambda (res a* x*) ({} pos-opt l)
    (let ((name (if (listp (car a*))  ;will be NIL if a* = NIL
                    (caar a*)
                    (car a*))))
      (cond ((null a*) (list res x*))
            ((null x*)                ;still have args after exhausting the list
             (if (listp (car a*))     ;check for default form
                 (recur (dict-conj res name (cadar a*))
                        (cdr a*)
                        [])
                 (error "destruct-pos-opt: not enough positional arguments")))
            (t
             (recur (dict-conj res name (car x*)) (cdr a*) (cdr x*)))))))

(defun destruct-kw (kw l)
  "Helper function that matches keyword args to values in l."
  (let ((pairs (group 2 l)))
    ;; make sure
    (mapc $(progn
             (unless (keywordp (car $)) ;check key/val syntax
               (error "destruct-kw: malformed keyword argument ~s" $))
             (unless (assoc (car $) kw) ;check legal key
               (error "destruct-kw: keyword doesn't match arg list ~s" (car $)))) 
          pairs)
    ;; make
    (reduce (lambda (res k)
              (aif (assoc (car k) pairs)              ;look for key
                   (dict-conj res (cadr k) (cadr it)) ;use value from l
                   (if (is-opt-arg (cdr k))           ;check for default value
                       (dict-conj res (cadr k) (caddr k))
                       (error
                        "destruct-kw: Missing required keyword argument ~s"
                        k))))
            kw
            :initial-value {})))

;; ;; here's what this looks like in fn (gotta keep eyes on the prize)
;; (defn destruct-pos-opt (pos-opt l)
;;   (rloop (res a* x*) ({} pos-opt l)
;;     (case a*
;;       [] [res x*]                       ;no more args
;;       [[a v] _]                         ;optional
;;         (recur (dict-conj res a (if x* (head x*) v))
;;                (tail a*)
;;                (tail x*))
;;       [a & _]                           ;non-optional
;;         (if x*
;;             (recur (dict-conj res a (head x*))
;;                    (tail a*)
;;                    (tail x*))
;;             (error "destruct-pos-opt: missing positional arguments")))))

(defun destruct-arg-obj (a l &optional (error-val nil error-val-p))
  "Create a key-value pairing of symbols named in args and values in l."
  (with-slots (pos opt kw rest has-kw has-rest) a
    (let* ((res-and (destruct-pos-opt (append pos opt) l))
           (res (car res-and))
           (l0 (cadr res-and)))
      (cond (has-rest (dict-conj res rest l0))
            (has-kw (dict-extend res (destruct-kw kw l0)))
            (t (if (null l0)
                   res
                   (if error-val-p
                       error-val
                       (error "destruct-arg-obj: extra arguments ~s" l0))))))))

(defun arg-list-vars (l)
  "Get the variable bound by an FN-style args list"
  (arg-obj-vars (make-args l)))

(defun destructure-arg-list (a* l &optional (error-val nil))
  "Create a key-value pairing of symbols named in args and values in l"
  (destruct-arg-obj (make-args a*) l error-val))


;;;;;;
;;; Function transformation and fn macro
;;;;;;

;;; supported options:
;; - :curry (INT or BOOL)
;; - :memoize (INT or BOOL)
;; - :doc STR (unused)
;; - :argdoc STR (unused)
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

;;; TODO: replace (here) with a macro that knows about arguments, doc strings,
;;; declare, etc
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

;;; TODO: teach curry-transform to know about argument names, doc strings,
;;; declarations
(defun curry-transform (curry-times lambda-form)
  "Wrap a lambda form to automatically curry the first several arguments."
  (declare (type integer curry-times))
  (let ((a* (gensym))
        (b* (gensym))
        (f (gensym))
        (self (gensym)))
    `(let ((,f ,lambda-form))
       (labels ((,self (&rest ,a*)
                  (if (<= (length ,a*) ,curry-times)
                      (lambda (&rest ,b*)
                        (apply #',self (append ,a* ,b*)))
                      (apply ,f ,a*))))
         #',self))))


(defmacro fn (arg-list &body body)
  "Like lambda, but with different argument list and support for options"
  (let* ((lform `(lambda ,(convert-arg-list arg-list) . ,body))
         (lopt (get-options body)))
    ;; IMPLNOTE: if at all possible, implement new options by expanding the
    ;; pipeline before
    (->> lform
      (funcall $(aif (dict-get lopt :memoize)
                     `(memoize ,$)
                     $))
      (funcall $(aif (dict-get lopt :curry)
                     (curry-transform it $)
                     $)))))

;; ;; test code (it's /almost/ unit tests lol)
;; (setf (symbol-function '+c) (fn (a & a*)
;;                               {:curry 1}
;;                               (apply #'+ a a*)))
;; (setf (symbol-function '+m) (fn (a & a*)
;;                                  {:memoize t}
;;                                  (sleep 2.0)
;;                                  (apply #'+ a a*)))
;; (setf (symbol-function '+slow) (fn (a & a*)
;;                                  {:curry 1 :memoize t}
;;                                  (sleep 2.0)
;;                                  (apply #'+ a a*)))
;; (setf (symbol-function 'test-args)
;;       (fn (a (b 'nothing) :c (c 'nothing))
;;         (format t "~s ~s ~s" a b c)))
;; (setf (symbol-function 'test-req-key)
;;       (fn (:req1 req1 :req2 req2 :opt1 (opt1 'unspec))
;;         (format t "~s, ~s, and (optional) ~s" req1 req2 opt1)))
