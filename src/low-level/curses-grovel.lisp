;; (include "/usr/include/ncurses.h")
#+use-curses-header
(include "curses.h")
#+use-pdcurses-header
(include "pdcurses.h")
#-(or use-curses-header use-pdcurses-header)
(include "ncurses.h")

(in-package #:cl-charms/low-level)

(ctype bool "bool") ;; grovel it, because sizeof(bool) is implementation-dependent

(ctype chtype "chtype")
(ctype attr_t "attr_t")
