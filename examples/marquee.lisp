(defpackage charms-marquee
  (:use :cl)
  (:export :main))
(in-package :charms-marquee)

(defclass marquee ()
  ((position :initarg :position :accessor marquee-position)
   (size :initarg :size :accessor marquee-size)
   (text :initarg :text :accessor marquee-text)
   (viewport :initarg :viewport :reader marquee-viewport)
   (padding-char :initarg :padding-char :initform #\Space :accessor marquee-padding-char)
   (buffer :initform "" :accessor marquee-buffer))
  (:documentation "A marquee is like a scrolling text thing. A string of characters moves through
a viewport. The viewport constrains the text so you only see a little bit at a time."))

(defmethod marquee-viewport :before ((marquee marquee))
  "Before getting the MARQUEE viewport (the text that is shown at a given time), the buffer
needs to be padded in case the text doesn't fill the MARQUEE size"
  (marquee-pad-buffer marquee))

(defmethod marquee-viewport ((marquee marquee))
  "Evaluates to the text currently shown in the viewport for the given MARQUEE."
  (with-slots (size buffer)
      marquee
    (subseq buffer 0 size)))

(defmethod marquee-pad-buffer ((marquee marquee))
  "When the size of the MARQUEE is greater than the supplied text, the buffer needs to be padded
with spaces."
  (with-accessors ((text marquee-text) (size marquee-size) (buffer marquee-buffer) (pad-char marquee-padding-char))
      marquee
    ;; The buffer size can never be less than the size of the text. Otherwise, the text won't show.
    (when (<= (length buffer) (length text))
      (setf buffer text))
    ;; This cond block pads the BUFFER. First condition is when the SIZE of the marquee is great than the TEXT. In this case,
    ;; extra spaces are added to the
    (cond ((> size (length buffer))
           (setf buffer (concatenate 'string text (make-string (- size (length text)) :initial-element pad-char))))
          ((and (< size (length buffer))
                (> size (length text)))
           (setf buffer (concatenate 'string text
                                     (make-string (- size (length text)) :initial-element pad-char)))))))

(defmethod marquee-advance-buffer ((marquee marquee) &optional (chars-to-advance 1))
  "Advances the buffer for the given MARQUEE. Optionally specify the number of CHARS-TO-ADVANCE."
  (with-accessors ((buffer marquee-buffer))
      marquee
    (setf buffer (concatenate 'string (subseq buffer chars-to-advance)
                              (subseq buffer 0 chars-to-advance)))))

(defmethod display-marquee ((window charms:window) (marquee marquee))
  "Show the given MARQUEE in the given WINDOW by writting out the contenst of the viewport."
  (with-accessors ((viewport marquee-viewport) (position marquee-position))
      marquee
    (destructuring-bind (x . y) position
       (charms:write-string-at-point window viewport x y))))

(defparameter *rate* 50)
(defparameter *last* 0)
(defparameter *current* nil)

(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (loop :named driver-loop
          :with marquee := (make-instance 'marquee :position '(0 . 0) :size 0
                                                   :text " Long live ncurses and cl-charms ")
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                (charms:clear-window charms:*standard-window*)

                (multiple-value-bind (width height)
                    (charms:window-dimensions charms:*standard-window*)
                  (setf (marquee-position marquee) (cons 0 (floor (/ height 2))))
                  (setf (marquee-size marquee) width))

                (display-marquee charms:*standard-window* marquee)
                (setq *current* (get-internal-real-time))
                (when (> (* 1000 (float (/ (- *current* *last*) internal-time-units-per-second))) *rate*)
                  (marquee-advance-buffer marquee)
                  (setq *last* *current*))
                (charms:refresh-window charms:*standard-window*)

                (case c
                  ((nil) nil)
                  ((#\q #\Q) (return-from driver-loop)))))))
