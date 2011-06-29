;; ecl script to compile a C file
(let ((quicklisp-init (merge-pathnames ".quicklisp.ecl/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(push (merge-pathnames ".asdf/" (user-homedir-pathname)) asdf:*central-registry*)
(dolist (lib '(:alexandria
               :com.gigamonkeys.binary-data
               :metabang-bind
               :split-sequence
               :cl-ppcre
               :diff))
  (require lib))
(require :cmp)
(require :elf)
(setf c::*compile-in-constants* t)
(c:build-shared-library "elf"
 :lisp-files (list (compile-file "requirements" :system-p t :verbose nil)
                   (compile-file "package" :system-p t :verbose nil)
                   (compile-file "util" :system-p t :verbose nil)
                   (compile-file "elf" :system-p t :verbose nil)))