;; (include "/usr/include/ncurses.h")
#-pdcurses (include #-(or windows win32) "ncurses.h"
                    #+(or windows win32) "ncursesw/ncurses.h")
#+pdcurses(include "pdcurses.h")

(in-package #:cl-charms/low-level)

(ctype bool "bool") ;; grovel it, because sizeof(bool) is implementation-dependent

(ctype chtype "chtype")
(ctype attr_t "attr_t")
