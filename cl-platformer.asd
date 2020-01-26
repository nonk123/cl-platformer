(defpackage cl-platformer-system
  (:use :common-lisp :asdf))

(in-package cl-platformer-system)

(defsystem "cl-platformer"
  :description "A simple platformer game."
  :author "nonk123"
  :version "0.1"
  :depends-on ("cl-opengl"
               "cl-glfw3"
               "3d-vectors"
               "3d-matrices"
               "trivial-main-thread")
  :license "MIT License"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "platformer")))
