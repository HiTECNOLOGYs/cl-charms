;;;; miscellaneous.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun beep-console ()
  "Audibly beep to alert the user."
  (check-status (charms/ll:beep))
  t)

(defun flash-console ()
  "Visually flash the console."
  (check-status (charms/ll:flash))
  t)

(defun curses-version ()
  "Return a string representing the version of the underlying curses implementation."
  (charms/ll:curses-version))
