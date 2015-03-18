;;;; This is an example that shows a simple "paint"/ASCII-art program.
;;;;
;;;; Move around the canvas with WASD
;;;;
;;;;    [W]: Up
;;;;    [A]: Left
;;;;    [S]: Down
;;;;    [D]: Right
;;;;
;;;; and paint to it/erase from it by pressing the space bar.
;;;;
;;;; The cursor will wrap around the edges like a torus.
(defpackage charms-paint
  (:use :cl)
  (:export :main))
(in-package :charms-paint)

(defun paint ()
  "Paint an asterisk at the cursor, or erase the one already painted."
  ;; We don't want to move the cursor when we paint.
  (charms:with-restored-cursor charms:*standard-window*
    (charms:write-char-at-cursor
     charms:*standard-window*
     (if (char/= #\Space (charms:char-at-cursor charms:*standard-window*))
         #\Space
         #\*))))

;;; Main driver

(defun main ()
  "Start the timer program."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (loop :named driver-loop
          :with x := 0                  ; Cursor X coordinate
          :with y := 0                  ; Cursor Y coordinate
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                ;; Refresh the window
                (charms:refresh-window charms:*standard-window*)

                ;; Process input
                (case c
                  ((nil) nil)
                  ((#\w) (decf y))
                  ((#\a) (decf x))
                  ((#\s) (incf y))
                  ((#\d) (incf x))
                  ((#\Space) (paint))
                  ((#\q #\Q) (return-from driver-loop)))

                ;; Normalize the cursor coordinates
                (multiple-value-bind (width height)
                    (charms:window-dimensions charms:*standard-window*)
                  (setf x (mod x width)
                        y (mod y height)))

                ;; Move the cursor to the new location
                (charms:move-cursor charms:*standard-window* x y)))))
