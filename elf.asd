;; Copyright (C) 2011  Eric Schulte

(asdf:defsystem elf
  :name "elf"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :license "GPL V3"
  :description "Common Lisp library for manipulation of ELF files."
  :version "0.1"
  :depends-on (alexandria
               com.gigamonkeys.binary-data
               metabang-bind
               split-sequence
               trivial-shell
               cl-ppcre
               diff ;; <- from http://github.com/froydnj/diff
               )
  :components ((:file "package")
               (:file "util"
                      :depends-on ("package"))
               (:file "elf"
                      :depends-on ("package"
                                   "util"))))
