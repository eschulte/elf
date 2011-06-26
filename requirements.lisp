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
