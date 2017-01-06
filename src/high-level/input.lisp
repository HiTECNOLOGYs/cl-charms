;;;; input.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun get-char (window &key ignore-error)
  "Get a character from the window WINDOW. In the event a character is not ready or could not be returned, thensignal an error. If IGNORE-ERROR is T, then instead return NIL."
  (let ((c (charms/ll:wgetch (window-pointer (resolve-window window)))))
    (cond
      ((not (eql c charms/ll:ERR)) (c-char-to-character c))
      (ignore-error nil)
      (t (error "Error getting character with GET-CHAR.")))))
