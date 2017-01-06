;;;; initialization.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:cl-charms)

(defun initialize ()
  "Initialize ncurses and the terminal for drawing. Return the standard window.

This function must be called before using curses functions. Consider using the macro `CHARMS:WITH-CURSES' to ensure this.
"
  (let ((stdscr (charms/ll:initscr)))
    (when (cffi:null-pointer-p stdscr)
      (error "INITIALIZE failed.")))

  ;; Query the standard window the normal way.
  (let ((win (standard-window)))
    (refresh-window win) ; Refresh window to update its dimensions
    win))

(defun finalize ()
  "Finalize ncurses.

This function must be called before exiting. Consider using the macro `CHARMS:WITH-CURSES' to ensure this."
  (check-status (charms/ll:endwin))
  t)

(defmacro with-curses (options &body body)
  "Execute the body BODY, ensuring that curses is properly initialized and finalized.

Within BODY, the special variable *STANDARD-WINDOW* will be bound, which refers to the global window.

Currently, there are no OPTIONS."
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
  (check-status (charms/ll:echo))
  t)

(defun disable-echoing ()
  "Disable the echoing of characters to the screen."
  (check-status (charms/ll:noecho))
  t)

(defun enable-extra-keys (window)
  "Enable extra keys, such as arrow and function keys, in the window WINDOW."
  (check-status (charms/ll:keypad (window-pointer (resolve-window window))
                                  charms/ll:TRUE))
  t)

(defun disable-extra-keys (window)
  "Disable extra keys, such as arrow and function keys, in the window WINDOW."
  (check-status (charms/ll:keypad (window-pointer (resolve-window window))
                                  charms/ll:FALSE))
  t)

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

(defun enable-non-blocking-mode (window)
  "Enable non-blocking mode for the window WINDOW. This will cause character input functions to not block and error (or return NIL)."
  (check-status (charms/ll:nodelay (window-pointer (resolve-window window))
                                   charms/ll:TRUE)))

(defun disable-non-blocking-mode (window)
  "Disable non-blocking mode for the window WINDOW. This will cause character input to block."
  (check-status (charms/ll:nodelay (window-pointer (resolve-window window))
                                   charms/ll:FALSE)))
