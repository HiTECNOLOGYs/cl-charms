(defpackage #:charms-marquee
  (:use #:cl)
  (:export #:main))
(in-package #:charms-marquee)

(defclass marquee ()
  ((position :initarg :position :accessor marquee-position)
   (size :initarg :size :accessor marquee-size)
   (text :initarg :text :accessor marquee-text)
   (padding-char :initarg :padding-char :accessor marquee-padding-char)
   (buffer :initform "" :accessor marquee-buffer))
  (:default-initargs :padding-char #\Space)
  (:documentation "A marquee is like a scrolling text thing. A string of characters moves
through a viewport. The viewport constrains the text so you only see a little
bit at a time."))

(defmethod marquee-viewport ((marquee marquee))
  "Evaluates to the text currently shown in the viewport for the given MARQUEE."
  (marquee-pad-buffer marquee)
  (subseq (marquee-buffer marquee) 0 (marquee-size marquee)))

(defmethod marquee-pad-buffer ((marquee marquee))
  "When the size of the MARQUEE is greater than the supplied text, the buffer
needs to be padded with the value stored in the marquee's padding-char slot,
whose initial value is a space."
  (with-accessors ((text marquee-text)
                   (size marquee-size)
                   (buffer marquee-buffer)
                   (padding-char marquee-padding-char))
      marquee
    ;; The buffer size can never be less than the size of the text. Otherwise,
    ;; the text won't show.
    (when (<= (length buffer) (length text))
      (setf buffer text))
    ;; This cond block pads the BUFFER. First condition is when the SIZE of
    ;; the marquee is great than the TEXT. In this case, extra spaces are
    ;; added to the
    (when (or (> size (length buffer))
              (< (length text) size (length buffer)))
      (let ((padding (make-string (- size (length text)) :initial-element padding-char)))
        (setf buffer (concatenate 'string text padding))))))

(defmethod marquee-advance-buffer ((marquee marquee) &optional (chars-to-advance 1))
  "Advances the buffer for the given MARQUEE. Optionally specify the number of CHARS-TO-ADVANCE."
  (with-accessors ((buffer marquee-buffer))
      marquee
    (setf buffer (concatenate 'string
                              (subseq buffer chars-to-advance)
                              (subseq buffer 0 chars-to-advance)))))

(defmethod display-marquee ((window charms:window) (marquee marquee))
  "Show the given MARQUEE in the given WINDOW by writting out the contents of the viewport."
  (with-accessors ((viewport marquee-viewport)
                   (position marquee-position))
      marquee
    (destructuring-bind (x . y) position
       (charms:write-string-at-point window viewport x y))))

(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (loop :named driver-loop
          ;; Marquee with position and size initialized as 0s because size and
          ;; position are updated programmatically later on.
          :with marquee := (make-instance 'marquee :position '(0 . 0) :size 0
                                                   :text " Long live ncurses and cl-charms ")
          :with last-timestamp := 0
          :with current-timestamp := nil
          :with rate = 50
          :for c := (charms:get-char charms:*standard-window*
                                     :ignore-error t)
          :do (progn
                (charms:clear-window charms:*standard-window*)

                (multiple-value-bind (width height)
                    (charms:window-dimensions charms:*standard-window*)
                  (setf (marquee-position marquee) (cons 0 (floor (/ height 2))))
                  (setf (marquee-size marquee) width))

                (display-marquee charms:*standard-window* marquee)
                (setq current-timestamp (get-internal-real-time))
                (when (> (* 1000 (float (/ (- current-timestamp last-timestamp) internal-time-units-per-second))) 50)
                  (marquee-advance-buffer marquee)
                  (setq last-timestamp current-timestamp))
                (charms:refresh-window charms:*standard-window*)

                (case c
                  ((nil) nil)
                  ((#\q #\Q) (return-from driver-loop)))))))
