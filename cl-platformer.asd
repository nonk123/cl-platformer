(defpackage cl-platformer-system
  (:use :common-lisp :asdf))

(in-package cl-platformer-system)

(defsystem "cl-platformer"
  :description "A simple platformer game."
  :author "nonk123"
  :version "0.1"
  :depends-on ("cl-glfw3")
  :license "MIT License"
  :components ((:file "package")
               (:file "platformer" :depends-on ("package"))))
