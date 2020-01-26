(in-package :cl-platformer)

(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun rad->deg (rad)
  (* rad (/ 180 pi)))

(defun vec2->vec3 (v2)
  (vec3 (vx v2) (vy v2) 0))
