;;;; package.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith <quad@symbo1ics.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage #:cl-charms
  (:documentation "Package containing a high-level interface to ncurses.")
  (:use #:cl)
  (:nicknames #:charms)
  
  ;; windows.lisp
  (:export
   #:window                             ; CLASS
   #:*standard-window*                  ; SPECIAL VARIABLE
   #:standard-window                    ; FUNCTION
   #:make-window                        ; FUNCTION
   #:destroy-window                     ; FUNCTION
   #:copy-window                        ; FUNCTION
   #:window-dimensions                  ; FUNCTION
   #:refresh-window                     ; FUNCTION
   #:force-repaint                      ; FUNCTION
   #:clear-window                       ; FUNCTION
   #:clear-window-after-cursor          ; FUNCTION
   #:clear-line-after-cursor            ; FUNCTION
   #:char-at-cursor                     ; FUNCTION
   #:char-at-point                      ; FUNCTION
   )
  
  ;; cursor.lisp
  (:export
   #:cursor-position                    ; FUNCTION
   #:move-cursor                        ; FUNCTION
   #:with-restored-cursor               ; MACRO
   #:move-cursor-up                     ; FUNCTION
   #:move-cursor-down                   ; FUNCTION
   #:move-cursor-right                  ; FUNCTION
   #:move-cursor-left                   ; FUNCTION
   )
  
  ;; output.lisp
  (:export
   #:insert-char-at-cursor              ; FUNCTION
   #:insert-char-at-point               ; FUNCTION
   #:write-char-at-cursor               ; FUNCTION
   #:write-string-at-cursor             ; FUNCTION
   #:write-char-at-point                ; FUNCTION
   #:write-string-at-point              ; FUNCTION
   )
  
  ;; input.lisp
  (:export
   #:get-char                           ; FUNCTION
   )
  
  ;; initialization.lisp
  (:export
   #:initialize                         ; FUNCTION
   #:finalize                           ; FUNCTION
   #:with-curses                        ; MACRO
   #:enable-echoing                     ; FUNCTION
   #:disable-echoing                    ; FUNCTION
   #:enable-extra-keys                  ; FUNCTION
   #:disable-extra-keys                 ; FUNCTION
   #:enable-raw-input                   ; FUNCTION
   #:disable-raw-input                  ; FUNCTION
   #:enable-non-blocking-mode           ; FUNCTION
   #:disable-non-blocking-mode          ; FUNCTION
   )
  
  ;; miscellaneous.lisp
  (:export
   #:beep-console                       ; FUNCTION
   #:flash-console                      ; FUNCTION
   #:curses-version                     ; FUNCTION
   )
  )
