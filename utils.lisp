(in-package :cl-platformer)

(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun rad->deg (rad)
  (* rad (/ 180 pi)))

(defun vbetween (v min max)
  (and (v>= v min) (v<= v max)))

(defun in-rect (point size)
  (vbetween point (vec 0 0) size))

(defun in-rect-offset (point offset size)
  (in-rect (v- point offset) size))

(defun rect-in (p1 s1 p2 s2)
  (and (v>= (v+ p1 s1) p2)
       (v<= p1 (v+ p2 s2))))
