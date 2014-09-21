;;;; output.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun write-char-at-cursor (window char)
  "Write the character CHAR to the window WINDOW at the cursor."
  (check-status
   (charms/ll:waddch (window-pointer window)
                     (character-to-c-char char)))
  t)

(defun write-string-at-cursor (window string)
  "Write the string STRING to the window WINDOW at the cursor."
  (check-status
   (charms/ll:waddstr (window-pointer window) string))
  t)

(defun write-char-at-point (window char x y)
  "Write the character CHAR to the window WINDOW at the coordinates (X, Y)."
  (check-status
   (charms/ll:mvwaddch (window-pointer window) y x
                       (character-to-c-char char)))
  t)

(defun write-string-at-point (window string x y)
  "Write the string STRING to the window WINDOW at the coordinates (X, Y)."
  (check-status
   (charms/ll:mvwaddstr (window-pointer window) y x string))
  t)
