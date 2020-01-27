(in-package cl-platformer)

(defclass window ()
  ((shader-program
    :initarg :shader-program
    :accessor shader-program
    :initform nil)
   (rectangle-vao
    :initarg :rectangle-vao
    :accessor rectangle-vao
    :initform nil)
   (rectangle-vbo
    :initarg :rectangle-vbo
    :accessor rectangle-vbo
    :initform nil)
   (rectangle-vertex-array
    :initarg :rectangle-vertex-array
    :accessor rectangle-vertex-array
    :initform nil)
   (vert-shader
    :initarg :vert-shader
    :accessor vert-shader
    :initform nil)
   (vert-shader-source
    :initarg :vert-shader-source
    :accessor vert-shader-source
    :initform "
#version 330 core

layout(location = 0) in vec4 pos;

out vec4 vertexColor;

uniform mat4 mvp = mat4(1.0);
uniform vec4 color = vec4(0.0, 0.0, 0.0, 0.0);

void main() {
    gl_Position = mvp * pos;
    vertexColor = color;
}
")
   (frag-shader
    :initarg :frag-shader
    :accessor frag-shader
    :initform nil)
   (frag-shader-source
    :initarg :frag-shader-source
    :accessor frag-shader-source
    :initform "
#version 330 core

layout(location = 0) out vec4 color;

in vec4 vertexColor;

void main() {
    color = vertexColor;
}
")))

(defvar +window+ (make-instance 'window))

(defclass entity ()
  ((pos
    :initarg :pos
    :accessor pos
    :initform (vec 0 0))
   (size
    :initarg :size
    :accessor size
    :initform (vec 0 0))
   (vel
    :initarg :vel
    :accessor vel
    :initform (vec 0 0))
   (col
    :initarg :col
    :accessor col
    :initform (vec 1.0 0.5 0.1))))

(defclass game ()
  ((keys
    :initarg :keys
    :accessor keys
    :initform (list))
   (entities
    :initarg :entities
    :accessor entities
    :initform (list))))

(defvar +game+ (make-instance 'game))

(defclass camera ()
  ((pos
    :initarg :pos
    :accessor pos
    :initform (vec 0 0 10))
   (vel
    :initarg :vel
    :accessor vel
    :initform (vec 0 0 0))))

(defvar +camera+ (make-instance 'camera))

(defun is-held (key)
  (member key (keys +game+)))

(defun entity= (a b)
  (and (v= (pos  a) (pos  b))
       (v= (size a) (size b))
       (v= (vel  a) (vel  b))))

(defun entity/= (a b)
  (not (entity= a b)))

(defun collide (a b)
  (rect-in (pos a) (size a) (pos b) (size b)))

(defun collide-all (a)
  (let ((result nil))
    (dolist (e (entities +game+) result)
      (let ((c (collide a e)))
        (when (and (entity/= a e) c)
          (progn
            (setq result c)
            (return)))))
    result))

(defgeneric update (thing)
  (:documentation "Update a THING."))

(defmethod update ((thing entity))
  (when (collide-all thing)
    (setf (col thing) (vec 1.0 0.0 0.0))))

(defmethod update ((thing camera))
  (let* ((i 0.1) (-i (- i)))
    (setf (vel thing)
          (vec (cond
                 ((is-held :d)  i)
                 ((is-held :a) -i)
                 (t 0.0))
               (cond
                 ((is-held :w)  i)
                 ((is-held :s) -i)
                 (t 0.0))
               (cond
                 ((is-held :r) -i)
                 ((is-held :f)  i)
                 (t 0.0)))))
  (nv+ (pos thing) (vel thing)))

(defgeneric render (thing)
  (:documentation "Render a THING."))

(defmethod render ((thing entity))
  (gl:use-program (shader-program +window+))
  (gl:uniformf
   (gl:get-uniform-location (shader-program +window+) "color")
   (vx (col thing))
   (vy (col thing))
   (vz (col thing))
   0)
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location (shader-program +window+) "mvp")
   (marr
    (let* ((ws (glfw:get-window-size))
           (ar (/ (elt ws 0) (elt ws 1)))
           (model
            (m* (mtranslation (pos thing))
                (mscaling (size thing))))
           (view (mtranslation (v- (pos +camera+))))
           (projection (mperspective 90 ar 0.01 100)))
      (m* projection view model))))
  (gl:draw-arrays :polygon 0 4))

(defparameter +rectangle-vertices+
  #(0.0 0.0
    1.0 0.0
    1.0 1.0
    0.0 1.0))

(defun tick ()
  (gl:clear-color 0.1 0.1 0.1 0)
  (gl:clear :color-buffer :depth-buffer)

  (when (or (is-held :q) (is-held :escape))
    (set-window-should-close))

  (update +camera+)

  (loop for e in (entities +game+)
     do (update e)
     do (render e))

  (swap-buffers)
  (poll-events))

(defun set-viewport (w h)
  (gl:viewport 0 0 w h))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun init ()
  (dotimes (i 10)
    (let* ((r 20.0)
           (s 1.5)
           (x (random r))
           (y (random r))
           (w (random s))
           (h (random s)))
      (push (make-instance 'entity
                           :pos (vec  (- (/ r 2) x)
                                      (- (/ r 2) y))
                           :size (vec (+ 1.0 w)
                                      (+ 1.0 h)))
            (entities +game+)))))

(def-key-callback input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (cond
    ((eq action :press)
     (push key (keys +game+)))
    ((eq action :release)
     (setf (keys +game+) (remove key (keys +game+))))))

(defun check-shader-error (shader name)
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (format t "~a: ~a~%" name error-string))))

(defun setup-shader ()
  (when (rectangle-vbo +window+)
    (gl:delete-buffers (list (rectangle-vbo +window+)))
    (setf (rectangle-vbo +window+) nil))

  (when (rectangle-vao +window+)
    (gl:delete-vertex-arrays (list (rectangle-vao +window+)))
    (setf (rectangle-vao +window+) nil))

  (when (rectangle-vertex-array +window+)
    (gl:free-gl-array (rectangle-vertex-array +window+))
    (setf (rectangle-vertex-array +window+) nil))

  (setf (rectangle-vertex-array +window+) (gl:alloc-gl-array :float 8))
  (loop for i from 0 to 7
     do (setf (gl:glaref (rectangle-vertex-array +window+) i)
              (elt +rectangle-vertices+ i)))

  (setf (rectangle-vao +window+) (gl:gen-vertex-array))
  (gl:bind-vertex-array (rectangle-vao +window+))
  (setf (rectangle-vbo +window+) (gl:gen-buffer))
  (gl:bind-buffer :array-buffer (rectangle-vbo +window+))
  (gl:buffer-data :array-buffer :static-draw (rectangle-vertex-array +window+))
  (gl:vertex-attrib-pointer 0 2 :float :false 0 0)
  (gl:enable-vertex-attrib-array 0)

  (setf (vert-shader +window+) (gl:create-shader :vertex-shader))
  (gl:shader-source (vert-shader +window+)
                    (vert-shader-source +window+))
  (gl:compile-shader (vert-shader +window+))
  (check-shader-error (vert-shader +window+) "vert")

  (setf (frag-shader +window+) (gl:create-shader :fragment-shader))
  (gl:shader-source (frag-shader +window+)
                    (frag-shader-source +window+))
  (gl:compile-shader (frag-shader +window+))
  (check-shader-error (frag-shader +window+) "frag")

  (setf (shader-program +window+) (gl:create-program))
  (gl:attach-shader (shader-program +window+) (vert-shader +window+))
  (gl:attach-shader (shader-program +window+) (frag-shader +window+))
  (gl:link-program (shader-program +window+)))

(defun cleanup ()
  (gl:use-program 0)
  (gl:detach-shader (shader-program +window+) (vert-shader +window+))
  (gl:detach-shader (shader-program +window+) (frag-shader +window+))
  (gl:delete-program (shader-program +window+))
  (gl:delete-shader (vert-shader +window+))
  (gl:delete-shader (frag-shader +window+))
  (setq +window+ (make-instance 'window))
  (setq +game+ (make-instance 'game))
  (setq +camera+ (make-instance 'camera)))

(defun run ()
  (let ((w 800) (h 600))
    (with-init-window (:title "Platformer" :width w :height h)
      (set-window-size-callback 'update-viewport)
      (set-key-callback 'input)
      (set-viewport w h)
      (setup-shader)
      (loop until (window-should-close-p)
         do (tick))
      (cleanup))))

(defvar +run-in-main-thread+ nil
  "t if the application must run in the main thread.")

(defun main ()
  (init)
  (if +run-in-main-thread+
      (with-body-in-main-thread () (run))
      (run))
  nil)
