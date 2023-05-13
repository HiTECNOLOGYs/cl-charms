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
    (setq *menu* (charms:make-menu
                  (list (charms:make-item :name "Hello"
                                          :description "Menu")
                        (charms:make-item :name "Goodbye" :description "Menu")
                        (charms:make-item :name "Happy" :description "Sauce"))))
    (let ((selected-item (charms:menu-select charms:*standard-window* *menu*)))
      (charms:write-string-at-point
       charms:*standard-window*
       (concatenate 'string
                    "You selected: "
                    (charms:item-name selected-item)
                    ", "
                    (charms:item-description selected-item)) 0 0))
    (loop :named driver-loop
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                (case c
                  ((#\q #\Q) (return-from driver-loop)))
                ))))
