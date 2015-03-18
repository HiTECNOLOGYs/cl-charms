(asdf:defsystem #:cl-charms-timer
  :version "0.1"
  :author "Robert Smith"
  :description "Timer example for cl-charms."
  :license "MIT License (See COPYING)"
  :depends-on (#:cl-charms)
  :serial t
  :pathname "examples/"
  :components ((:file "timer")))
