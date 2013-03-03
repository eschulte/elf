;; Copyright (C) 2011-2013 Eric Schulte
(defsystem :elf
  :name "elf"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :license "GPL V3"
  :description "Common Lisp library for manipulation of ELF files."
  :version "0.1"
  :depends-on (alexandria
               com.gigamonkeys.binary-data
               metabang-bind
               split-sequence
               #-ecl trivial-shell
               cl-ppcre)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "elf" :depends-on ("package" "util"))))
