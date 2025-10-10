#!/bin/bash

cd /Users/key-person/projects/lisp-blog

sbcl --non-interactive \
  --load setup-cache.lisp \
  --eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
  --eval '(handler-case (ql:quickload :lisp-blog-test :silent t) (error (e) (format t "Load error: ~A~%" e) (sb-ext:exit :code 1)))' \
  --eval '(in-package :lisp-blog-test)' \
  --eval '(run-tests)'
