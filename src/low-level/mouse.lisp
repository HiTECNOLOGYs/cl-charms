(in-package #:cl-charms/low-level)

(define-exported-constant BUTTON1_RELEASED            #x1)
(define-exported-constant BUTTON1_PRESSED             #x2)
(define-exported-constant BUTTON1_CLICKED             #x4)
(define-exported-constant BUTTON1_DOUBLE_CLICKED      #x8)
(define-exported-constant BUTTON1_TRIPLE_CLICKED     #x10)

(define-exported-constant BUTTON2_RELEASED           #x40)
(define-exported-constant BUTTON2_PRESSED            #x80)
(define-exported-constant BUTTON2_CLICKED           #x100)
(define-exported-constant BUTTON2_DOUBLE_CLICKED    #x200)
(define-exported-constant BUTTON2_TRIPLE_CLICKED    #x400)

(define-exported-constant BUTTON3_RELEASED         #x1000)
(define-exported-constant BUTTON3_PRESSED          #x2000)
(define-exported-constant BUTTON3_CLICKED          #x4000)
(define-exported-constant BUTTON3_DOUBLE_CLICKED   #x8000)
(define-exported-constant BUTTON3_TRIPLE_CLICKED  #x10000)

(define-exported-constant BUTTON_CTRL           #x1000000)
(define-exported-constant BUTTON_SHIFT          #x2000000)
(define-exported-constant BUTTON_ALT            #x4000000)

(define-exported-constant ALL_MOUSE_EVENTS      #x7ffffff)
(define-exported-constant REPORT_MOUSE_POSITION #x8000000)

;;==============================================================================
;; C-prototype: mmask_t mousemask(mmask_t newmask, mmask_t *oldmask);
(cffi:defcfun ("mousemask" %mousemask) :unsigned-long
  "To make mouse events visible, use the mousemask function.  This will set the
mouse events to be reported.  By default, no mouse events are reported.  The
function will return a mask to indicate which of the specified mouse events can
be reported; on complete failure it returns 0.  If oldmask is non-NULL, this
function fills the indicated location with the previous value of the given
window's mouse event mask.

As a side effect, setting a zero mousemask may turn off the mouse pointer;
setting a nonzero mask may turn it on.  Whether this happens is
device-dependent."
  (newmask :unsigned-long)
  (oldmask :pointer))
;;------------------------------------------------------------------------------
(defun mousemask (newmask)
  "set newmask as mousemask, returning old one"
  (cffi:with-foreign-object (oldmask :unsigned-long)
    (%mousemask newmask oldmask)
    (cffi:mem-ref oldmask :unsigned-long)))
(export 'mousemask)

;;==============================================================================
;; C-prototype: typedef struct
;;        {
;; 	   short id;	     /* ID to distinguish multiple devices */
;; 	   int x, y, z;      /* event coordinates */
;; 	   mmask_t bstate;   /* button state bits */
;;        }
;;        MEVENT;
(cffi:defcstruct mevent
    "Point structure."
    (id :short)
    (x :int)
    (y :int)
    (z :int)
    (bstate :unsigned-long))

;; C-prototype: int getmouse(MEVENT *event);
(cffi:defcfun ("getmouse" %getmouse) :int
  "Once a class of mouse events has been made visible in a window, calling the
wgetch function on that window may return KEY_MOUSE as an indicator that a mouse
event has been queued.  To read the event data and pop the event off the queue,
call getmouse.  This function will return OK if a mouse event is actually
visible in the given window, ERR otherwise.  When getmouse returns OK, the data
deposited as y and x in the event structure coordinates will be screen-relative
character-cell coordinates.  The returned state mask will have exactly one bit
set to indicate the event type.  The corresponding data in the queue is marked
invalid.  A subsequent call to getmouse will retrieve the next older item from
the queue."
  (event :pointer))

;; C-prototype: int ungetmouse(MEVENT *event);
(cffi:defcfun ("ungetmouse" %ungetmouse) :int
  "The ungetmouse function behaves analogously to ungetch.  It pushes a
KEY_MOUSE event onto the input queue, and associates with that event the given
state data and screen-relative character-cell coordinates."
  (event :pointer))

(defun getmouse ()
  (cffi:with-foreign-object (ptr '(:struct mevent))
    (let ((result (%getmouse ptr)))
      ;; Terminal reports all mouse events (depending on terminal mode which may
      ;; be customized with escape sequences) and they are returned from getch
      ;; and curses library can't do anything about it - if such event is masked
      ;; with mousemask then result is ERR because there are no events waiting
      ;; in queue when getmouse is called. This is expected. -- 2018-02-02 jd
      (when (eql result ERR) (error "Error in curses call getmouse."))
      (cffi:with-foreign-slots ((id x y z bstate) ptr (:struct mevent))
        (values bstate x y z id)))))
(export 'getmouse)

;; C-prototype: bool wenclose(const WINDOW *win, int y, int x);
(define-exported-cfuns ("wenclose") :bool
  "The wenclose function tests whether a given pair of screen-relative
character-cell coordinates is enclosed by a given window, returning TRUE if it
is and FALSE otherwise.  It is useful for determining what subset of the screen
windows enclose the location of a mouse event."
  (win window-ptr)
  (y :int)
  (x :int))

;; C-prototype: bool mouse_trafo(int* pY, int* pX, bool to_screen);
(define-exported-cfuns ("mouse_trafo") :bool
  "The mouse_trafo function performs the same translation as wmouse_trafo, using
stdscr for win."
  (py :pointer)
  (px :pointer)
  (to-screen :bool))

;; C-prototype: bool wmouse_trafo(const WINDOW* win, int* pY, int* pX, bool to_screen);
(define-exported-cfuns ("mouse_wtrafo") :bool
  "The wmouse_trafo function transforms a given pair of coordinates from
stdscr-relative coordinates to coordinates relative to the given window or vice
versa.  The resulting stdscr-relative coordinates are not always identical to
window-relative coordinates due to the mechanism to reserve lines on top or
bottom of the screen for other purposes (see the ripoffline and slk_init calls,
for example).

- If the parameter to_screen is TRUE, the pointers pY, pX must reference the
  coordinates of a location inside the window win.  They are converted to
  window-relative coordinates and returned through the pointers.  If the
  conversion was successful, the function returns TRUE.

- If one of the parameters was NULL or the location is not inside the window,
  FALSE is returned.

- If to_screen is FALSE, the pointers pY, pX must reference window-relative
  coordinates.  They are converted to stdscr-relative coordinates if the window
  win encloses this point.  In this case the function returns TRUE.

- If one of the parameters is NULL or the point is not inside the window, FALSE
  is returned.  The referenced coordinates are only replaced by the converted
  coordinates if the transformation was successful."
  (win window-ptr)
  (py :pointer)
  (px :pointer)
  (to-screen :bool))

;; C-prototype: int mouseinterval(int erval);
(define-exported-cfuns ("mouseinterval") :int
  "The mouseinterval function sets the maximum time (in thousands of a second)
that can elapse between press and release events for them to be recognized as a
click.  Use mouseinterval(0) to disable click resolution.  This function returns
the previous interval value.  Use mouseinterval(-1) to obtain the interval
without altering it. The default is one sixth of a second."
  (erval :int))

;; C-prototype: bool has_mouse(void);
(define-exported-cfuns ("has_mouse") :bool
  "The has_mouse function returns TRUE if the mouse driver has been successfully
initialized.")
