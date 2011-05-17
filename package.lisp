;; Copyright (C) 2011  Eric Schulte

(defpackage #:elf
  (:use
   :common-lisp
   :alexandria
   :com.gigamonkeys.binary-data
   :metabang-bind
   :split-sequence
   :trivial-shell
   :cl-ppcre
   :org.tfeb.hax.memoize)
  (:export
   ;; functions
   :bytes-to-int :int-to-bytes :named-section :elf-p :read-elf :write-elf
   :show-dynamic :show-symbols :show-file-layout :show-memory-layout
   :mapslots :generic-copy :copy-elf :named-symbol :symbols
   ;; disassembly functions
   :objdump :parse-addresses :objdump-parse
   ;; methods
   :un-type :ptr  :val :binding :type :offset  :vma 
   :size  :type  :flags :alignment :read-value :write-value
   :address :link :info :addralign :entsize :vaddr :paddr
   :filesz :memsz :align :sym-name :other :value :shndx :disasm
   :rel-sym :rel-type :rel-info
   ;; section class
   :elf :sh :ph :name :data
   ;; elf class
   :header :section-table :program-table :sections :ordering))
