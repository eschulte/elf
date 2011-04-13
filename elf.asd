;; Copyright (C) 2011  Eric Schulte

(asdf:defsystem elf
  :name "elf"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :components ((:file "elf"))
  :depends-on (alexandria
               com.gigamonkeys.binary-data
               metabang-bind
               split-sequence
               trivial-shell
               cl-ppcre))
