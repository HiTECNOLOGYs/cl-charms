;; (include "/usr/include/ncurses.h")
(include "ncurses.h")
(in-package #:cl-charms/low-level)

(ctype bool "bool") ;; grovel it, because sizeof(bool) is implementation-dependent
