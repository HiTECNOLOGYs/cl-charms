;;;; windows.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defclass window ()
  ((pointer :initarg :pointer
            :accessor window-pointer
            :documentation "Pointer to the underlying representation of a window pointer. (This is of CFFI type `CHARMS/LL:WINDOW-PTR'.)"))
  (:documentation "A curses window."))

(defvar *standard-window*)
(setf (documentation '*standard-window* 'variable)
      "Bind-only special variable containing the standard window.")

(let ((%cached-window (make-instance 'window :pointer (cffi:null-pointer))))
  (defun standard-window ()
    (flet ((updated-cached-window ()
             (let ((stdscr charms/ll:*stdscr*))
               ;; Update the cached window if necessary.
               (unless (cffi:pointer-eq stdscr (window-pointer %cached-window))
                 (when (cffi:null-pointer-p stdscr)
                   (error "No standard window exists. Did you initialize correctly?"))
                 (setf (window-pointer %cached-window) stdscr))
               ;; Return the possibly-updated cached window.
               %cached-window)))
      (if (boundp '*standard-window*)
          *standard-window*
          (updated-cached-window)))))

(defun make-window (width height start-x start-y)
  "Make a new window of width WIDTH and height HEIGHT, starting at the coordinate (START-X, START-Y).

Note that windows may not overlap."
  ;; FIXME: The behavior of this function is special if WIDTH or
  ;; HEIGHT are 0. Either document this behavior or disallow it.
  ;;
  ;; We should check if this window overlaps with another, and
  ;; appropriately warn.
  (let ((pointer (charms/ll:newwin height width start-y start-x)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate a new window."))
    (make-instance 'window :pointer pointer)))

(defun destroy-window (window)
  "Destroy the window WINDOW."
  (check-status (charms/ll:delwin (window-pointer window)))
  (slot-makunbound window 'pointer)
  t)

(defun copy-window (window)
  "Copy the window WINDOW."
  (let ((new-pointer (charms/ll:dupwin (window-pointer window))))
    (when (cffi:null-pointer-p new-pointer)
      (error "Failed to copy the window ~S." window))
    (make-instance 'window :pointer new-pointer)))

(defun window-dimensions (window)
  "Given a window WINDOW, return its width and height as two values respectively."
  (let (width height)
    (charms/ll:getmaxyx (window-pointer window) height width)
    (values width height)))

(defun refresh-window (window)
  "Refresh the display of the window WINDOW."
  (check-status (charms/ll:wrefresh (window-pointer window)))
  t)

(defun char-at-cursor (window)
  "What is the character at the cursor in the window WINDOW?"
  (c-char-to-character (charms/ll:winch (window-pointer window))))

(defun char-at-point (window x y)
  "What is the character at the point (X, Y) in the window WINDOW?"
  (c-char-to-character (charms/ll:mvwinch (window-pointer window) y x)))
