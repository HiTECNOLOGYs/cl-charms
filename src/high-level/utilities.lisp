;;;; utilities.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun to-foreign-boolean (value)
  "Convert the value VALUE to a foreign bool."
  (if value
      charms/ll:TRUE
      charms/ll:FALSE))

(defun from-foreign-boolean (value &key relax)
  "Convert the value VALUE to a Lisp boolean. If RELAX is true, then all non-false values are true."
  (if relax
      (not (eql value charms/ll:FALSE))
      (cond
        ((eql value charms/ll:FALSE) nil)
        ((eql value charms/ll:TRUE)    t)
        (t (error "The value ~S is not a valid foreign boolean." value)))))

(defun character-to-c-char (character)
  "Convert a Lisp character to a C character."
  (check-type character character)
  ;; FIXME: This isn't quite right.
  (char-code character))

(defun c-char-to-character (c-char)
  "Convert a C character to a Lisp character."
  ;; FIXME: This isn't quite right.
  (code-char c-char))

(defmacro check-status (form)
  "Check the status of the resulting value VALUE to see if it is an error. If so, signal an error. If not, return the value."
  (let ((error-string (if (and (listp form)
                               (symbolp (first form)))
                          (format nil "Error in curses call from function ~S (received ERR)." (first form))
                          "Error in curses call (received ERR).")))
    `(%check-status ,form :error-message ',error-string)))

(defun %check-status (value &key error-message)
  (if (eql value charms/ll:ERR)
      (error (or error-message "Error in curses call."))
      value))

(declaim (special *standard-window*))
(declaim (inline resolve-window))
(defun resolve-window (window-designator)
  (if (eq t window-designator)
      *standard-window*
      window-designator))
