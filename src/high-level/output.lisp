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


;;; acs_map isn't filled until initialization, so
;;; this function will get them (VT100 symbols) at runtime.
(defun get-from-acs-map (acs-name)
  "Get the ACS-NAME, found in cl-charms/low-level from the acs-map. 
Note that this should be used with the write-acs-... functions."
  (check-status
   (cffi:mem-aref (cffi:get-var-pointer 'cl-charms/low-level:*ACS-MAP*) :int (char-int acs-name))))


(defun write-acs-char-at-cursor (window acs-name)
  "Write the ACS-NAME to the window WINDOW at the cursor.
Returns nil acs-name is invalid."
  (let ((decimal-repr (char-int acs-name)))
	(when (or (>= decimal-repr 128) (< decimal-repr 0)); acs_map is of size 128, thus we need to check the bounds.
			  (return-from write-acs-char-at-cursor nil)))
  (check-status
   (charms/ll:waddch (window-pointer window)
                     (get-from-acs-map acs-name)))
  t)

(defun write-acs-char-at-point (window acs-name x y)
  "Write the character CHAR to the window WINDOW at the coordinates (X, Y)."
(let ((decimal-repr (char-int acs-name)))
	(when (or (>= decimal-repr 128) (< decimal-repr 0)); acs_map is of size 128, thus we need to check the bounds.
			  (return-from write-acs-char-at-point nil)))
  (check-status
   (charms/ll:mvwaddch (window-pointer window) y x
                       (get-from-acs-map acs-name)))
  t)

