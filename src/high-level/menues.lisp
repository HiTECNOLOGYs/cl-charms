;;;; menu.lisp
;;;;
;;;; Copyright (c) 2023 Roygbyte

(in-package #:cl-charms)

(defclass menu ()
  ((pointer :initarg :pointer
            :accessor menu-pointer
            :documentation "Pointer to the underlying representation of a menu pointer."))
  (:documentation "A curses menu."))

(defun make-menu (window items)
  (let ((item-ptrs (cffi:foreign-alloc :pointer :count (length items) :null-terminated-p t))
        (menu-ptr nil))
    (dotimes (i (length items))
      (setf (cffi:mem-aref item-ptrs :pointer i)
            (item-pointer (nth i items))))
    (setq menu-ptr (charms/ll:new-menu item-ptrs))
    (when (cffi:null-pointer-p menu-ptr)
      (error "Failed to allocate a new menu."))
    (charms/ll:set-menu-win menu-ptr (window-pointer (resolve-window window)))
    (charms/ll:post-menu menu-ptr)
    (make-instance 'menu :pointer menu-ptr)
    menu-ptr))

(defun with-menu (window menu-ptr)
  (loop :named while-loop
        :do (progn
             (charms:refresh-window window)
              (case (charms:get-char window :ignore-error t)
                ((nil) nil)
                ((#\t) (charms/ll:menu-driver menu-ptr charms/ll:REQ_UP_ITEM))
                ((#\n) (charms/ll:menu-driver menu-ptr charms/ll:REQ_DOWN_ITEM))
                ((#\q) (return-from while-loop)))
              (sleep 0.1))))

(defclass item ()
  ((pointer :initarg :pointer
            :accessor item-pointer
            :documentation "Pointer to the underlying representation of an item."))
  (:documentation "A curses menu item"))

(defun make-item (name description)
  (let* ((name-ptr (cffi:foreign-string-alloc name))
         (description-ptr (cffi:foreign-string-alloc description))
         (pointer (charms/ll:new-item name-ptr description-ptr)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate a new item."))
    (make-instance 'item :pointer pointer)))
