;; Copyright (C) 2011  Eric Schulte

(defpackage #:elf
  (:use
   :common-lisp
   :alexandria
   :com.gigamonkeys.binary-data
   :metabang-bind
   :split-sequence
   #-ecl :trivial-shell
   :cl-ppcre
   :flexi-streams
   )
  (:shadow
   :class
   :get
   :type)
  (:export
   ;; dynamic variables
   :*calculate-edits* :*endian* :*class*
   ;; functions
   :bytes-to-int :int-to-bytes :named-section :elf-p :elf-header
   :read-elf :write-elf
   :show-dynamic :show-symbols :show-file-layout :show-memory-layout
   :mapslots :generic-copy :copy-elf :named-symbol :symbols
   ;; disassembly functionality
   :disassemblable :objdump :csurf :project :disassemble-section
   :objdump-cmd :objdump :parse-addresses :objdump-parse
   :csurf-cmd
   :csurf-script
   ;; methods
   :un-type :ptr  :val :binding :offset  :vma 
   :size  :type  :flags :alignment :read-value :write-value
   :address :link :info :addralign :entsize :vaddr :paddr
   :filesz :memsz :align :sym-name :other :value :shndx :disasm
   :rel-sym :rel-type :rel-info :magic-number
   ;; section class
   :elf :sh :ph :name :data
   ;; elf class
   :header :section-table :program-table :sections :ordering
   ))
