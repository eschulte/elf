;; Copyright (C) 2013  Eric Schulte
(defpackage #:elf-test
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :elf
   :stefil
   :trivial-timeout)
  (:shadow :type)
  (:export :run-elf-tests :test))
