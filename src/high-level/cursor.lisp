;;;; cursor.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun cursor-position (window)
  "Given a window WINDOW, return its X and Y coordinates as two values respectively."
  (let (x y)
    (charms/ll:getyx (window-pointer (resolve-window window)) y x)
    (values x y)))

(defun move-cursor (window x y)
  "Move the cursor in window WINDOW to the coordinates (X, Y)."
  (check-status (charms/ll:wmove (window-pointer (resolve-window window)) y x))
  t)

(defmacro with-restored-cursor (window &body body)
  "Execute the body BODY, restoring the cursor position in the window WINDOW to its beginning state."
  (let ((gwindow (gensym "WINDOW-"))
        (cursor-x (gensym "CURSOR-X"))
        (cursor-y (gensym "CURSOR-Y")))
    `(let ((,gwindow (resolve-window ,window)))
       (multiple-value-bind (,cursor-x ,cursor-y)
           (cursor-position ,gwindow)
         (multiple-value-prog1 (progn ,@body)
           (move-cursor ,gwindow ,cursor-x ,cursor-y))))))

(defun move-cursor-up (window &key (amount 1))
  "Move the cursor in the window WINDOW up by 1 character. If the positive integer AMOUNT is specified, it will be moved up AMOUNT characters. If negative, it will move down AMOUNT characters."
  (check-type amount integer)
  (let ((window (resolve-window window)))
    (multiple-value-bind (x y) (cursor-position window)
      (move-cursor window
                   x
                   (max 0 (- y amount))))))

(defun move-cursor-down (window &key (amount 1))
  "Move the cursor in the window WINDOW down by 1 character. If the positive integer AMOUNT is specified, it will be moved down AMOUNT characters. If negative, it will move up AMOUNT characters."
  (check-type amount integer)
  (let ((window (resolve-window window)))
    (multiple-value-bind (x y) (cursor-position window)
      (move-cursor window
                   x
                   (max 0 (+ y amount))))))

(defun move-cursor-right (window &key (amount 1))
  "Move the cursor in the window WINDOW right by 1 character. If the positive integer AMOUNT is specified, it will be moved right AMOUNT characters. If negative, it will move left AMOUNT characters."
  (check-type amount integer)
  (let ((window (resolve-window window)))
    (multiple-value-bind (x y) (cursor-position window)
      (move-cursor window
                   (max 0 (+ x amount))
                   y))))

(defun move-cursor-left (window &key (amount 1))
  "Move the cursor in the window WINDOW left by 1 character. If the positive integer AMOUNT is specified, it will be moved left AMOUNT characters. If negative, it will move right AMOUNT characters."
  (check-type amount integer)
  (let ((window (resolve-window window)))
    (multiple-value-bind (x y) (cursor-position window)
      (move-cursor window
                   (max 0 (- x amount))
                   y))))
