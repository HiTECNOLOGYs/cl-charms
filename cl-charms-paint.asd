(asdf:defsystem #:cl-charms-paint
  :version "0.1"
  :author "Robert Smith"
  :description "A simple ASCII art paint program for cl-charms."
  :license "MIT License (See COPYING)"
  :depends-on (#:cl-charms)
  :serial t
  :pathname "examples/"
  :components ((:file "paint")))
