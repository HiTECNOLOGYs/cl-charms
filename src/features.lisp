;;;; features.lisp

(in-package #:cl)

;;; Based on a snippet taken from UIOP (MIT license)
#+(or allegro clasp clisp clozure cmucl ecl mkcl mkcl sbcl)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (when (and #+allegro (member :ics *features*)
             #+(or clasp clisp cmucl ecl mkcl) (member :unicode *features*)
             #+clozure (member :openmcl-unicode-strings *features*)
             #+sbcl (member :sb-unicode *features*))
    ;; Check for unicode at runtime, so that a hypothetical FASL compiled with unicode
    ;; but loaded in a non-unicode setting (e.g. on Allegro) won't tell a lie.
    (pushnew :unicode *features*)))
