;;;; attributes.lisp
;;;;
;;;;


;;; Look into assume_default_colors extension
(defun initialize-pair (pair foreground &optional (background charms/ll:color_black))
  "Initialize color pair PAIR to colors FOREGROUND and BACKGROUND. 
BACKGROUND is optional and defaults to charms/ll:color_black."
  (check-status (charms/ll:init-pair pair foreground background)))


(defun initialize-color (color r g b)
  "Change definition of color COLOR with the RGB value values R, G, and B 
if the terminal supports changing colors. When init-color is called, all 
occurences of COLOR are immediately changed."
  (check-status (if (eq charms/ll:true (charms/ll:can-change-color))
					(charms/ll:init-color color r g b)
					nil)))

(defun attribute-on (&rest attributes)
  "Turn on the named attributes ATTRIBUTES without affecting any others."
  (check-status (charms/ll:attron (apply #'logior attributes))))


(defun attribute-on-window (window &rest attributes)
  "Turn on the named attributes ATTRIBUTES without affecting any others
for window WINDOW."
  (check-status (charms/ll:wattron (window-pointer window) (apply #'logior attributes))))


(defun attribute-off (&rest attributes)
  "Turn off the named attributes ATTRIBUTES without affecting any others."
  (check-status (charms/ll:attroff (apply #'logior attributes))))


(defun attribute-off-window (window &rest attributes)
  "Turn off the named attributes ATTRIBUTES without affecting any others
for window WINDOW."
  (check-status (charms/ll:wattroff (window-pointer window) (apply #'logior attributes))))

;;; #define COLOR_PAIR(n) NCURSES_BITS(n,0)
;;; #define NCURSES_BITS(mask,shift) ((mask) << ((shift) + NCURSES_ATTR_SHIFT))
(defun get-color-pair (n)
  "Return the color pair N to be used with an attribute function."
	(ash n 8))
