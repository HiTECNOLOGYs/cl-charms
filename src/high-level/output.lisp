;;;; output.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun write-char-at-cursor (window char)
  "Write the character CHAR to the window WINDOW at the cursor."
  (charms/ll:waddch (window-pointer window)
                    (character-to-c-char char)))

(defun write-char-at-point (window char x y)
  "Write the character CHAR to the window WINDOW at the coordinates (X, Y)."
  (charms/ll:mvwaddch (window-pointer window) y x
                      (character-to-c-char char)))

