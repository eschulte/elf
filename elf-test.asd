(defsystem :elf-test
  :description "Test the elf library."
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on
  (alexandria metabang-bind elf stefil trivial-timeout)
  :components
  ((:static-file "COPYING")
   (:file "package-test")
   (:file "util")
   (:file "util-test" :depends-on ("util" "package-test"))
   (:file "elf-test" :depends-on ("package-test" "util-test"))))
