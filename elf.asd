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
               cl-ppcre
               flexi-streams)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "elf" :depends-on ("package" "util"))
               (:file "arm" :depends-on ("package" "util" "elf"))
               (:file "instruction" :depends-on ("package"))
               (:file "disassemblable"
                      :depends-on ("package" "util" "elf" "instruction"))))
(defsystem :elf-test
  :description "Test the elf library."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind elf stefil trivial-timeout)
  :components
  ((:static-file "COPYING")
   (:module "test"
            :components
            ((:file "package")
             (:file "elf-test" :depends-on ("package"))))))
