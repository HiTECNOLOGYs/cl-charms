;; (include "/usr/include/ncurses.h")
#-(or windows win32) (include "ncurses.h")
#+(or windows win32) (include "pdcurses.h")

(in-package #:cl-charms/low-level)

(ctype bool "bool") ;; grovel it, because sizeof(bool) is implementation-dependent

(ctype chtype "chtype")
(ctype attr_t "attr_t")
