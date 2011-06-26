;; ecl script to compile a C file
(require :cmp)
(require :elf)
(setf c::*compile-in-constants* t)
(c:build-shared-library "elf"
 :lisp-files (list (compile-file "package" :system-p t :verbose nil)
                   (compile-file "util" :system-p t :verbose nil)
                   (compile-file "elf" :system-p t :verbose nil)))
