(asdf:defsystem #:cl-charms-marquee
  :version "0.1"
  :author "Scarlett McAllister"
  :description "A program that horizontally scrolls text across the screen."
  :license "MIT License (See COPYING)"
  :depends-on (#:cl-charms)
  :serial t
  :pathname "examples/"
  :components ((:file "marquee")))
