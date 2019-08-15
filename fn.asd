(defpackage :fn-asd
  (:use :cl :asdf))

(in-package :fn-asd)

(defsystem fn
  :name "fn"
  :version "0.0.0"
  :author "Jack Pugmire"
  :license "GPL v3.0"
  :description "fn Programming Language"
  :serial t
  :pathname "src"
  :components ((:file "util")
               (:file "dollar-sign-reader")
               (:file "test")
               (:file "scanner")
               (:file "ast")
               (:file "parser-gen")
               (:file "parser")
               (:file "values")
               (:file "code")
               (:file "eval")
               (:file "main")))
