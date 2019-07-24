#!/bin/sh

sbcl --load fn.asd \
     --eval '(ql:quickload :fn)' \
     --eval '(sb-ext:save-lisp-and-die #p"fn" :toplevel #'"'"'fn.main:main :executable t)'

