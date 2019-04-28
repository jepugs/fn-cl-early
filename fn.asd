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
  :components ((:file "package")
               (:file "util")
               (:file "dollar-sign-reader")
               (:file "constants")
               (:file "type")
               (:file "match")
               (:file "lexical")
               (:file "fn")
               (:file "protocol")
               (:file "bootlib")
               (:file "reader")
               (:file "bootstrap")))
