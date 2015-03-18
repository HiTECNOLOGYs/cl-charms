;;;; This is an example that shows a simple timer using the high-level
;;;; interface. Run MAIN and press the space bar to start, stop, and clear the
;;;; timer. Press 'q' to quit.
(defpackage charms-timer
  (:use :cl)
  (:export :main))
(in-package :charms-timer)

;;; Timer state & manipulation

(defvar *start* nil)
(defvar *stop* nil)

(defun start/stop/clear ()
  "Start, stop, and clear the timer successively."
  (cond
    (*stop*
     (setf *start* nil
           *stop* nil))
    ((not *start*)
     (setf *stop* nil
           *start* (get-internal-real-time)))
    (t
     (setf *stop* (get-internal-real-time)))))

(defun time-elapsed ()
  "Compute the time elapsed since *START* (to *END* if set). If the timer hasn't started, return NIL."
  (and *start*
       (/ (- (or *stop* (get-internal-real-time))
             *start*)
          internal-time-units-per-second)))

;;; Rendering function

(defun paint-time ()
  "Paint the elapsed time to the center of the screen."
  (multiple-value-bind (width height)
      (charms:window-dimensions charms:*standard-window*)
    (let* ((dt (time-elapsed))
           (printed-time (if dt
                             (format nil "~,2F" dt)
                             "Press [SPACE] to start/stop/clear"))
           (length/2 (floor (length printed-time) 2)))
      (charms:write-string-at-point charms:*standard-window*
                                    printed-time
                                    (- (floor width 2) length/2)
                                    (floor height 2)))))

;;; Main driver

(defun main ()
  "Start the timer program."
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (loop :named driver-loop
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                ;; Redraw time
                (charms:clear-window charms:*standard-window*)
                (paint-time)
                (charms:refresh-window charms:*standard-window*)

                ;; Process input
                (case c
                  ((nil) nil)
                  ((#\Space) (start/stop/clear))
                  ((#\q #\Q) (return-from driver-loop)))))))
