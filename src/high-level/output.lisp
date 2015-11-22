;;;; output.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun insert-char-at-cursor (window char)
  "Insert the character CHAR at the cursor within the window WINDOW, advancing the rest of the line, without moving the cursor. (This is akin to pressing the 'insert' key and typing a character.)"
  (check-status
   (charms/ll:winsch (window-pointer window)
                     (character-to-c-char char)))
  t)

(defun insert-char-at-point (window char x y)
  "Insert the character CHAR at the coordinates (X,Y) within the window WINDOW, advancing the rest of the line, without moving the cursor. (This is akin to pressing the 'insert' key and typing a character.)"
  (check-status
   (charms/ll:mvwinsch (window-pointer window)
                       y
                       x
                       (character-to-c-char char)))
  t)

;;; Is the coordinate (X, Y) the last position within the window
;;; WINDOW?
(defun last-position-p (window x y)
  (multiple-value-bind (width height)
      (window-dimensions window)
    (and (= x (1- width))
         (= y (1- height)))))

;;; Write the character CHAR at the last position of the window
;;; WINDOW. This assumes that the width of the window is at least 2.
(defun write-char-at-last-position (window char)
  (multiple-value-bind (width height)
      (window-dimensions window)
    (let* ((last-x (1- width))
           (last-y (1- height)))
      (insert-char-at-point window char last-x last-y))))

(defun write-char-at-cursor (window char)
  "Write the character CHAR to the window WINDOW at the cursor."
  (multiple-value-bind (x y)
      (cursor-position window)
    (if (last-position-p window x y)
        (write-char-at-last-position window char)
        (check-status
         (charms/ll:waddch (window-pointer window)
                           (character-to-c-char char)))))
  t)

(defun write-string-at-cursor (window string)
  "Write the string STRING to the window WINDOW at the cursor."
  (check-status
   (charms/ll:waddstr (window-pointer window) string))
  t)

(defun write-char-at-point (window char x y)
  "Write the character CHAR to the window WINDOW at the coordinates (X, Y)."
  (if (last-position-p window x y)
      (write-char-at-last-position window char)
      (check-status
       (charms/ll:mvwaddch (window-pointer window) y x
                           (character-to-c-char char))))
  t)

(defun write-string-at-point (window string x y)
  "Write the string STRING to the window WINDOW at the coordinates (X, Y)."
  (check-status
   (charms/ll:mvwaddstr (window-pointer window) y x string))
  t)
