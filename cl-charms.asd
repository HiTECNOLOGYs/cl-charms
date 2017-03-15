;;;; This file is part of cl-charms, providing CFFI bindings to libcurses.
;;;;
;;;; Copyright (c) 2014 Mark Fedurin <hitecnologys@gmail.com>
;;;; Copyright (c) 2010 Abhishek Reddy <http://abhishek.geek.nz>
;;;;
;;;; This file includes portions of code from cl-ncurses, an ncurses
;;;; interface for Common Lisp.  The copyright notices from cl-ncurses
;;;; are reproduced below.
;;;;
;;;; Copyright (c) 2003 Nikodemus Siivola
;;;; Copyright (c) 2004 Marcelo Ramos <mramos@montevideo.com.uy>
;;;; Copyright (c) 2007 Jacob Gabrielson <jacobg23@pobox.com>
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

(asdf:defsystem #:cl-charms
  :version (:read-file-form "version.lisp-expr")
  :maintainer "Mark Fedurin <hitecnologys@gmail.com>"
  :author "Abhishek Reddy <abhishek@abhishek.geek.nz>"
  :description "CFFI bindings for curses."
  :license "MIT License (See COPYING)"
  :depends-on (#:cffi
               #:alexandria)
  :defsystem-depends-on (#:cffi-grovel) ; for processing :cffi-grovel-file
  :serial t
  :pathname "src/"
  :components ((:module "low-level"
                :serial t
                :components ((:file "package")
                             (:cffi-grovel-file "curses-grovel")
                             (:file "curses-bindings")
			     (:file "mouse")))
               (:module "high-level"
                :serial t
                :components ((:file "package")
                             (:file "utilities")
                             (:file "windows")
                             (:file "cursor")
                             (:file "output")
                             (:file "input")
                             (:file "initialization")
                             (:file "miscellaneous")))))
