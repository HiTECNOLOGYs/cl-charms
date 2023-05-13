;;;; menu.lisp
;;;;
;;;; Copyright (c) 2023 Roygbyte

(in-package #:cl-charms)

(defclass menu ()
  ((pointer :initarg :pointer
            :accessor menu-pointer
            :documentation "Pointer to the underlying representation of a menu pointer.")
  (items :initarg :items
         :accessor menu-items
         :documentation "Representation of the items in this menu."))
  (:documentation "A curses menu."))

(defun make-menu (items)
  (let ((item-ptrs (cffi:foreign-alloc :pointer :count (length items) :null-terminated-p t))
        (menu-ptr nil))
    (dotimes (i (length items))
      (setf (cffi:mem-aref item-ptrs :pointer i)
            (item-pointer (nth i items))))
    (setq menu-ptr (charms/ll:new-menu item-ptrs))
    (when (cffi:null-pointer-p menu-ptr)
      (error "Failed to allocate a new menu."))
    (make-instance 'menu :pointer menu-ptr :items items)))

(defun menu-select (window menu)
  (charms/ll:set-menu-win (menu-pointer menu) (window-pointer (resolve-window window)))
  (charms/ll:post-menu (menu-pointer menu))
  (charms:disable-non-blocking-mode charms:*standard-window*)
  (let ((val (loop :named while-loop
                   :with menu-ptr := (menu-pointer menu)
                   :for c := (charms:get-char window :ignore-error t :no-delay-mode t)
                   :do (progn
                         (case c
                           ((nil) nil)
                           ((#\t) (charms/ll:menu-driver menu-ptr charms/ll:REQ_UP_ITEM))
                           ((#\n) (charms/ll:menu-driver menu-ptr charms/ll:REQ_DOWN_ITEM))
                           ((#\h) (charms/ll:menu-driver menu-ptr charms/ll:REQ_LEFT_ITEM))
                           ((#\l) (charms/ll:menu-driver menu-ptr charms/ll:REQ_RIGHT_ITEM))
                           ((#\Return #\Newline)
                            (return-from while-loop
                              (resolve-menu-item-from-pointer (charms/ll:current-item menu-ptr) menu)))
                           ((#\q) (return-from while-loop)))
                         (charms:refresh-window window)
                         ))))
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (charms/ll:unpost-menu (menu-pointer menu))
    val)
  )

(defun resolve-menu-item-from-pointer (item-ptr menu)
  (dolist (item (menu-items menu))
    (if (cffi:pointer-eq item-ptr (item-pointer item))
        (return item))))

(defclass item ()
  ((pointer :initarg :pointer
            :accessor item-pointer
            :documentation "Pointer to the underlying representation of an item.")
   (name-ptr :initarg :name-ptr
             :accessor item-name-ptr)
   (description-ptr :initarg :description-ptr
             :accessor item-description-ptr))
  (:documentation "A curses menu item"))

(defgeneric item-name (item)
  (:method (item)
    (concatenate 'string (loop for i from 0 below (cffi:foreign-funcall "strlen" :pointer (item-name-ptr item) :int)
                               collect (code-char (cffi:mem-ref (item-name-ptr item) :char i))))))

(defgeneric item-description (item)
  (:method (item)
    (concatenate 'string (loop for i from 0 below (cffi:foreign-funcall "strlen" :pointer (item-description-ptr item) :int)
                 collect (code-char (cffi:mem-ref (item-description-ptr item) :char i))))))

(defun make-item (&key name description)
  (let* ((name-ptr (cffi:foreign-string-alloc name))
         (description-ptr (cffi:foreign-string-alloc description))
         (pointer (charms/ll:new-item name-ptr description-ptr)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate a new item."))
    (make-instance 'item :pointer pointer :name-ptr name-ptr :description-ptr description-ptr)))
