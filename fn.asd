(defpackage #:fn-asd
  (:use :cl :asdf))

(in-package :fn-asd)

(defsystem fn
  :name "fn"
  :version "0.0.0"
  :author "Jack Pugmire"
  :license "GPL v3.0"
  :description "fn Programming Language"
  :serial t
  :components ((:file "package-impl")
               (:file "macros")
               (:file "dict")
               (:file "schema")
               (:file "match")
               (:file "lexical")
               (:file "fn")
               (:file "bootlib")
               (:file "reader")))
