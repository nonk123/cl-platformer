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

void main() {
    gl_Position = mvp * pos;
    vertexColor = vec4(1.0, 0.5, 0.1, 1.0);
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

(defparameter +window+ (make-instance 'window))

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
    :initform (vec 0 0))))

(defclass game ()
  ((entities
    :initarg entities
    :accessor entities
    :initform (list))))

(defparameter +game+ (make-instance 'game))

(defclass camera ()
  ((pos
    :initarg :pos
    :accessor pos
    :initform (vec 0 0 0))
   (vel
    :initarg :vel
    :accessor vel
    :initform (vec 0 0 0))))

(defparameter +camera+ (make-instance 'camera))

(defgeneric entity= (a b)
  (:documentation "Determine if entities A and B are equal."))

(defmethod entity= ((a entity) (b entity))
  (and (v= (pos  a) (pos  b))
       (v= (size a) (size b))
       (v= (vel  a) (vel  b))))

(defgeneric render (thing)
  (:documentation "Render a THING."))

(defparameter +rectangle-vertices+
  #(0.0 0.0
    1.0 0.0
    1.0 1.0
    0.0 1.0))

(defmethod render ((thing entity))
  (gl:use-program (shader-program +window+))
  (gl:uniform-matrix-4fv
   (gl:get-uniform-location (shader-program +window+) "mvp")
   (marr
    (let* ((ws (glfw:get-window-size))
           (ar (/ (elt ws 0) (elt ws 1)))
           (model
            (m* (mscaling (size thing))
                (mtranslation (pos thing))))
           (view (mtranslation (v- (pos +camera+))))
           (projection
            (let* ((w 7) (h (/ w ar)))
              (mortho (- w) w (- h) h 0.01 50))))
      (m* view model projection))))
  (gl:draw-arrays :polygon 0 4))

(defparameter +keys+ (list)
  "List of held keys.")

(defun is-held (key)
  (member key +keys+))

(defun tick ()
  (gl:clear-color 0.1 0.1 0.1 0)
  (gl:clear :color-buffer :depth-buffer)

  (when (is-held :q)
    (set-window-should-close))

  (let* ((i 0.03) (-i (- i)))
    (setf (vel +camera+)
          (vec (cond
                 ((is-held :d)  i)
                 ((is-held :a) -i)
                 (t 0.0))
               (cond
                 ((is-held :w)  i)
                 ((is-held :s) -i)
                 (t 0.0))
               (cond
                 ((is-held :r)  i)
                 ((is-held :f) -i)
                 (t 0.0)))))

  (nv+ (pos +camera+) (vel +camera+))

  (loop for e in (entities +game+)
     do (render e))

  (swap-buffers)
  (poll-events))

(defun set-viewport (w h)
  (gl:viewport 0 0 w h))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(defun init ()
  (push (make-instance 'entity :pos (vec 0.0  0.0) :size (vec 0.8 2.5))
        (entities +game+))
  (push (make-instance 'entity :pos (vec 0.0 -0.3) :size (vec 0.8 0.8))
        (entities +game+)))

(def-key-callback input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (cond
    ((eq action :press)
     (push key +keys+))
    ((eq action :release)
     (setq +keys+ (remove key +keys+)))))

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
  (setf (shader-program +window+) nil)
  (setf (vert-shader +window+) nil)
  (setf (frag-shader +window+) nil)
  (setq +keys+ (list))
  (setf (vel +camera+) (vec 0 0 0))
  (setf (pos +camera+) (vec 0 0 0)))

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

(defparameter +run-in-main-thread+ nil
  "t if the application must run in the main thread.")

(defun main ()
  (init)
  (if +run-in-main-thread+
      (with-body-in-main-thread () (run))
      (run))
  nil)
