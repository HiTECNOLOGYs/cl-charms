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
(define-exported-constant REPORT_MOUSE_POSITION #x8000000)

;;==============================================================================
;; C-prototype: mmask_t mousemask(mmask_t newmask, mmask_t *oldmask);
(cffi:defcfun ("mousemask" %mousemask) :unsigned-long
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
  (event :pointer))

(defun getmouse ()
  (cffi:with-foreign-object (ptr '(:struct mevent))
    (let ((result (%getmouse ptr)))
      (declare (ignore result));; TODO: how to check results?
      (cffi:with-foreign-slots ((id x y z bstate) ptr (:struct mevent))
	;;(charms::%check-status result ); fails
	(values bstate x y z id)))))
(export 'getmouse)
