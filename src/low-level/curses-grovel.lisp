#+(and (not use-menu-h)
       (not (or windows win32)))
(include "ncurses.h")

#+(and (not use-menu-h)
       (or windows win32))
(include "pdcurses.h")

#+use-menu-h
(progn
  (include "/usr/include/menu.h")
  (cc-flags "-lmenu -lncurses"))

(in-package #:cl-charms/low-level)

(ctype bool "bool") ;; grovel it, because sizeof(bool) is implementation-dependent

(ctype chtype "chtype")
(ctype attr_t "attr_t")
