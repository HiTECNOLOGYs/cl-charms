;;;; initialization.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(alexandria:define-constant +standard-window+
    (make-instance 'window :pointer charms/ll:*stdscr*)
  :test (lambda (x y)
          (and (typep x 'window) (slot-boundp x 'pointer)
               (typep y 'window) (slot-boundp y 'pointer)
               (cffi:pointer-eq (window-pointer x)
                                (window-pointer y))))
  :documentation "The standard/global window.")

(defun initialize ()
  "Initialize ncurses and the terminal for drawing.

This function must be called before using curses functions. Consider using the macro `CHARMS:WITH-CURSES' to ensure this.
"
  (charms/ll:initscr))

(defun finalize ()
  "Finalize ncurses.

This function must be called before exiting. Consider using the macro `CHARMS:WITH-CURSES' to ensure this."
  (charms/ll:endwin))

(defmacro with-curses (options &body body)
  "Execute the body BODY, ensuring that "
  (assert (null options)
          (options)
          "Currently no options to WITH-CURSES-INITIALIZED are supported.")
  `(unwind-protect
        (progn
          (force-output *terminal-io*)  ; Should *TERMINAL-IO* be
                                        ; used?
          (initialize)
          (let ((*standard-window* (standard-window)))
            ,@body))
     (finalize)))

(defun enable-echoing ()
  "Enable the echoing of characters to the screen."
  (charms/ll:echo))

(defun disable-echoing ()
  "Disable the echoing of characters to the screen."
  (charms/ll:noecho))

(defun enable-extra-keys (window)
  "Enable extra keys, such as arrow and function keys, in the window WINDOW."
  (charms/ll:keypad (window-pointer window) charms/ll:TRUE))

(defun disable-extra-keys (window)
  "Disable extra keys, such as arrow and function keys, in the window WINDOW."
  (charms/ll:keypad (window-pointer window) charms/ll:FALSE))

(defvar *input-mode* nil)

(defun enable-raw-input (&key interpret-control-characters)
  "Enables raw input mode. This disables line buffering and will make characters available as soon as they're typed.

If INTERPRET-CONTROL-CHARACTERS is T, then control characters like Ctrl-C will be interpreted as usual."
  ;; Ensure mutual exclusion of :RAW and :CBREAK.
  (disable-raw-input)
  
  ;; Enable and remember the mode.
  (cond
    (interpret-control-characters
     (check-status (charms/ll:cbreak))
     (setf *input-mode* :cbreak)
     t)
    (t
     (check-status (charms/ll:raw))
     (setf *input-mode* :raw))))

(defun disable-raw-input ()
  "Disables raw input mode. This undoes the action of `CHARMS:ENABLE-RAW-INPUT'."
  (when (null *input-mode*)
    (check-status (charms/ll:nocbreak))
    (check-status (charms/ll:noraw)))
  
  (when (eq :raw *input-mode*)
    (check-status (charms/ll:noraw)))
  
  (when (eq :cbreak *input-mode*)
    (check-status (charms/ll:nocbreak)))
  
  t)

;; TODO: ENABLE-DELAY, DISABLE-DELAY

