;;;; This is an example that shows how to use menu.h.
(pushnew :use-menu-h *features*)

(defpackage #:charms-menu
  (:use #:cl)
  (:export #:main))
(in-package #:charms-menu)

(defvar *item*)
(defvar *menu*)

(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (setq *menu* (charms:make-menu
                  charms:*standard-window*
                  (list (charms:make-item "Hello" "Menu")
                        (charms:make-item "Goodbye" "Menu"))))
    (charms:with-menu charms:*standard-window* *menu*)))
