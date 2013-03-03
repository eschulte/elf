;; Copyright (C) 2013  Eric Schulte
(defpackage #:elf-test
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :elf
   :stefil
   :trivial-shell
   :trivial-timeout)
  (:shadow :type)
  (:export :test))
