(in-package #:gfx)

;; (declaim (optimize
;;           (debug 3)
;;           (speed 0)
;;           (safety 3)
;;           (space 0)))

(setf *kernel* (make-kernel 6))
(defparameter *channel* (make-channel))

(defstruct camera
  position
  yaw
  pitch)

(defstruct basis
  forward
  right
  up)

(defstruct face
  "Stores index of the vertices in *vertices*"
  
  (v0 0 :type fixnum)
  (v1 0 :type fixnum)
  (v2 0 :type fixnum))

(defparameter +camera-forward-initial+ (v! 1.0 0.0 0.0))
(defparameter +camera-right-initial+ (v! 0.0 0.0 1.0))
(defparameter +camera-position-initial+ (v! -100.0 0.0 1.0))
(defparameter +bg+ (color! 255 255 255 255))
(defparameter +fg+ (color! 0 0 0 255))
(defparameter +world-up+ (v! 0.0 1.0 0.0))
(defparameter *camera-speed* 5)
(defparameter +near-plane+ 0.1)
(defparameter +epsilon+ -1e-6)
(defparameter +pixel+ (color! 0 255 0 255))

(defparameter *debug-points* nil)
(defparameter *framebuffer-height* 600)
(defparameter *framebuffer-width* 600)
(defparameter *vertices* (make-array 2000 :adjustable t :fill-pointer 0))
(defparameter *projected-vertices* (make-array 2000 :adjustable t :fill-pointer 0)) ; 1 indexed
(defparameter *faces* (make-array 2000 :adjustable t :fill-pointer 0 :element-type 'face))
(defparameter *framebuffer* (make-array (list *framebuffer-width* *framebuffer-height*) :initial-element +bg+))
(defparameter *camera* (make-camera :position +camera-position-initial+
				    :yaw 0.0
				    :pitch 0.0))
(defparameter *focal-length* 866.0)
(defparameter *mouse-sensitivity* 1)
(defparameter *camera-looking* +camera-forward-initial+)

(defparameter *backface-culled-faces* (make-array 2000 :adjustable t :fill-pointer 0))

;; load vertices & faces from file
(with-open-file (obj-stream "sponza.obj")
  (setf (fill-pointer *vertices*) 0)
  (setf (fill-pointer *faces*) 0)
  (setf (fill-pointer *projected-vertices*) 0)
  (loop for line = (read-line obj-stream nil)
	while line
	doing
	   (when (<= 2 (length line))
	     (cond ((string= "v " line :end1 2 :end2 2)
		    (with-input-from-string (s (subseq line 2))
		      (let ((x (read s))
			    (y (read s))
			    (z (read s)))
			(vector-push-extend (v! x y z) *vertices*))))
		   ((string= "f " line :end1 2 :end2 2) ; collect only the vertex indices for faces
		    (with-input-from-string (s (subseq line 2))
		      (vector-push-extend (apply #'make-face
						 (loop repeat 3
						       for v-vt-vn = (symbol-name (read s))
						       for axis in '(:v0 :v1 :v2)
						       appending
						       (list
							axis
							(parse-integer v-vt-vn 
								       :junk-allowed t))))
					  *faces*)))))))

(defun transform-vector3(v m)
  "Transform a Vector3 by *draw-transform* (treated as w = 1)."
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14) m matrix
    (make-vector3
     :x (+ (* m0 (vector3-x v)) (* m4 (vector3-y v)) (* m8 (vector3-z v)) m12)
     :y (+ (* m1 (vector3-x v)) (* m5 (vector3-y v)) (* m9 (vector3-z v)) m13)
     :z (+ (* m2 (vector3-x v)) (* m6 (vector3-y v)) (* m10 (vector3-z v)) m14))))

(defun pixel(x y color)
  (when (and (<= 0 x (1- *framebuffer-width*))
	     (<= 0 y (1- *framebuffer-height*)))
    (setf (aref *framebuffer* x y) color)))

(defun bresenham (x0 y0 x1 y1 plot)
  (declare (type fixnum x0 y0 x1 y1))
  (let* ((dx (abs (- x1 x0)))
         (sx (if (< x0 x1) 1 -1))
         (dy (- (abs (- y1 y0))))
         (sy (if (< y0 y1) 1 -1))
         (err (+ dx dy)))
    (loop
      (funcall plot x0 y0)
      (when (and (= x0 x1) (= y0 y1))
        (return))
      (let ((e2 (* 2 err)))
        (when (>= e2 dy)
          (incf err dy)
          (incf x0 sx))
        (when (<= e2 dx)
          (incf err dx)
          (incf y0 sy))))))

(defun camera-basis()
  (with-members (yaw pitch) *camera* camera
    (let* ((yaw-rotated-initial (normalize
				 (transform-vector3 +camera-forward-initial+
						     (rotate-y-matrix! yaw))))
	   (right-direction (normalize
			     (cross yaw-rotated-initial +world-up+)))
	   (forward-direction (rotate-vector-about-axis yaw-rotated-initial right-direction (- pitch)))
	   (up-direction (normalize (cross right-direction forward-direction))))
      (make-basis :forward forward-direction :right right-direction :up up-direction))))

(defun camera-forward()
  (basis-forward (camera-basis)))

(defun camera-up()
  (basis-up (camera-basis)))

(defun camera-right()
  (basis-right (camera-basis)))

(defun display(painter)
  (with-window *framebuffer-width* *framebuffer-height* "Framebuffer"
    (%set-target-fps 30)
    (%disable-cursor)
    (with-members (position yaw pitch) *camera* camera
      (setf position +camera-position-initial+
	    yaw 0.0
	    pitch 0.0))
    (with-foreign-object (c-framebuffer '(:struct %color) (* *framebuffer-width* *framebuffer-height*))
      (with-texture framebuffer *framebuffer-width* *framebuffer-height*
	(labels ((repaint()
		   (funcall painter)
		   (%clear-background :white)
		   (dotimes (x-index *framebuffer-width*)
		     (dotimes (y-index *framebuffer-height*)
		       (let* ((mapped-y (- (1- *framebuffer-height*) y-index))
			      (lisp-color (aref *framebuffer* x-index y-index))
			      (c-color (mem-aptr c-framebuffer '(:struct %color) (+ (* mapped-y *framebuffer-width*) x-index))))
			 (with-members ((r cr) (g cg) (b cb) (a ca)) lisp-color color
			   (with-foreign-slots ((r g b a) c-color (:struct %color))
			     (setf r cr
				   g cg
				   b cb
				   a ca))))))
		   (%update-texture (render-texture-texture framebuffer) c-framebuffer)))
	  (repaint)
	  (loop until (%window-should-close)
		for dt = (%get-frame-time)
		doing
		   (with-keys ((:w :down forward)
			       (:a :down left)
			       (:s :down backward)
			       (:d :down right)
			       (:left_control :down down)
			       (:space :down up))
		     (with-members (position) *camera* camera
		       (let* ((forward-direction (camera-forward))
			      (forward-scaled (vector* (* *camera-speed* dt)
						       forward-direction)))
			 (with-members ((x fx) (y fy) (z fz)) forward-scaled vector3
			   (when forward
			     (let ((forward-matrix (translate-matrix! fx fy fz)))
			       (setf position
				     (transform-vector3
				      position
				      forward-matrix))))
			   (when backward
			     (let ((backward-matrix (translate-matrix! (- fx) (- fy) (- fz))))
			       (setf position
				     (transform-vector3
				      position
				      backward-matrix))))))
		       (let* ((right-direction (camera-right))
			      (right-scaled (vector* (* *camera-speed* dt)
						     right-direction)))
			 (with-members ((x rx) (y ry) (z rz)) right-scaled vector3
			   (when right
			     (let ((right-matrix (translate-matrix! rx ry rz)))
			       (setf position
				     (transform-vector3
				      position
				      right-matrix))))
			   (when left
			     (let ((left-matrix (translate-matrix! (- rx) (- ry) (- rz))))
			       (setf position
				     (transform-vector3
				      position
				      left-matrix))))))
		       (let* ((up-direction +world-up+)
			      (up-scaled (vector* (* *camera-speed* dt)
						  up-direction)))
			 (with-members ((x ux) (y uy) (z uz)) up-scaled vector3
			   (when up
			     (let ((up-matrix (translate-matrix! ux uy uz)))
			       (setf position
				     (transform-vector3
				      position
				      up-matrix))))
			   (when down
			     (let ((down-matrix (translate-matrix! (- ux) (- uy) (- uz))))
			       (setf position
				     (transform-vector3
				      position
				      down-matrix)))))))

		     (with-mouse ((:delta :x delta-x :y delta-y))
		       (with-members(yaw pitch) *camera* camera
			 (incf yaw (* delta-x *mouse-sensitivity* dt))
			 (wrapf yaw 360.0)
			 (incf pitch (* delta-y *mouse-sensitivity* dt))
			 (clampf pitch 90.0 -90.0))
		       (when (or forward backward left right up down (not (zerop delta-x)) (not (zerop delta-y)))
			 (repaint))))
		   (with-drawing
		     (%clear-background :white)
		     (with-math-coordinates (0 *framebuffer-height*)
		       (%draw-texture (render-texture-texture framebuffer)
				      0
				      0
				      :white))
		     (draw-fps 10 10)
		     (with-members (yaw pitch) *camera* camera
		       (draw-text (format nil "Yaw ~a~%Pitch ~a" yaw pitch) 10 80 20 :blue)))))))))

(defun clear-framebuffer()
  (dotimes (x *framebuffer-width*)
    (dotimes (y *framebuffer-height*)
      (setf (aref *framebuffer* x y) +bg+))))

(defun wireframe()
  (loop for f across *faces*
	doing
	   (with-members ((position camera-position)) *camera* camera
	     (with-members ((v0 v0-index) (v1 v1-index) (v2 v2-index)) f face
	       (let* ((start (aref *projected-vertices* v0-index))
		      (mid (aref *projected-vertices* v1-index))
		      (end (aref *projected-vertices* v2-index)))
		 (loop for (p0 p1) in
		       `((,start ,mid)
			 (,mid ,end)
			 (,end ,start))
		       while (and start mid end)
		       doing
			  (let* ((v1 (aref *vertices* (1- v0-index)))
				 (v2 (aref *vertices* (1- v1-index)))
				 (v3 (aref *vertices* (1- v2-index)))
				 (v3-to-v2 (vector-sub v2 v3))
				 (v2-to-v1 (vector-sub v1 v2))
				 (face-normal (cross v3-to-v2 v2-to-v1))
				 (face-to-cam (vector-sub camera-position v1)) ; any of the vectors will do
				 (dotted (dot face-normal face-to-cam))
				 (is-facing-camera? (> dotted 0)))
			    (if is-facing-camera?
				(with-members ((x x0) (y y0)) p0 vector2
				  (with-members ((x x1) (y y1)) p1 vector2
				    (bresenham (truncate x0)
					       (truncate y0)
					       (truncate x1)
					       (truncate y1)
					       #'(lambda(x-p y-p)
						   (pixel x-p y-p +fg+)))))))))))))

(defun backface-cull()
  (loop for f across *faces*
	  initially (setf (fill-pointer *backface-culled-faces*) 0)
	  initially (adjust-array *backface-culled-faces* (length *faces*))
	with v1-to-v2 = (make-vector3)
	with v1-to-v3 = (make-vector3)
	with face-normal = (make-vector3)
	with face-to-cam = (make-vector3)
	doing
	   (with-members ((position camera-position)) *camera* camera
	     (with-members ((v0 v0-index) (v1 v1-index) (v2 v2-index)) f face
	       (let* ((start (aref *projected-vertices* v0-index))
		      (mid (aref *projected-vertices* v1-index))
		      (end (aref *projected-vertices* v2-index)))
		 (if  (and start mid end)
		      (let* ((v1 (aref *vertices* (1- v0-index)))
			     (v2 (aref *vertices* (1- v1-index)))
			     (v3 (aref *vertices* (1- v2-index))))
			(vector-sub-into v1-to-v2 v2 v1)
			(vector-sub-into v1-to-v3 v3 v1)
			(cross-into face-normal v1-to-v2 v1-to-v3)
			(vector-sub-into face-to-cam camera-position v1) ; any of the vectors will do
			(if (> (dot face-normal face-to-cam) 0.0)
			    (vector-push-extend f *backface-culled-faces*)))))))))


(defun solve-linear-2x2 (l1 l2 r)
  (with-members ((x a) (y d)) l1 vector2
    (with-members ((x b) (y e)) l2 vector2
      (with-members ((x c) (y f)) r vector2
	(let* ((det (- (* a e) (* b d))))
	  (assert (/= det 0))
	  (make-vector2
	   :x (/ (- (* c e) (* b f)) det)
	   :y (/ (- (* a f) (* c d)) det)))))))


(defun rasterize()
  (loop for f across *backface-culled-faces*
	doing
	   (with-members ((v0 v0-index) (v1 v1-index) (v2 v2-index)) f face
	     (let* ((v1 (aref *projected-vertices* v0-index))
		    (v2 (aref *projected-vertices* v1-index))
		    (v3 (aref *projected-vertices* v2-index)))
	       (and v1 v2 v3
		    (with-members ((x v1-x) (y v1-y)) v1 vector2
		      (with-members ((x v2-x) (y v2-y)) v2 vector2
			(with-members ((x v3-x) (y v3-y)) v3 vector2
 			  (let* ((top (ceiling (max v1-y v2-y v3-y)))
				 (bottom (floor (min v1-y v2-y v3-y)))
				 (left (floor (min v1-x v2-x v3-x)))
				 (right (ceiling (max v1-x v2-x v3-x)))
				 (e1x (- v2-x v1-x))
				 (e1y (- v2-y v1-y))
				 (e2x (- v3-x v1-x))
				 (e2y (- v3-y v1-y))
				 (det (- (* e1x e2y) (* e2x e1y))))
			    (loop for px from left to right do
			      (loop for py from bottom to top
				    for tox = (- px v1-x)
				    for toy = (- py v1-y)
				    for beta = (/ (- (* tox e2y) (* e2x toy)) det)
				    for gamma = (/ (- (* e1x toy) (* tox e1y)) det)
				    for alpha = (- 1 beta gamma +epsilon+)
				    when (and (plusp alpha)
					      (plusp beta)
					      (plusp gamma))
				      do (pixel px py +pixel+))))))))))))



(display #'(lambda()
	     (loop 		     
	       initially (clear-framebuffer)
 		 initially (adjust-array *projected-vertices* (1+ (length *vertices*)))
 		 initially (setf (fill-pointer *projected-vertices*) (1+ (length *vertices*)))
	       with view-matrix = (with-members(x y z) (camera-position *camera*) vector3
				    (with-members(forward right up) (camera-basis) basis
				      (with-members ((x fx) (y fy) (z fz)) forward vector3
					(with-members ((x rx) (y ry) (z rz)) right vector3
					  (with-members ((x ux) (y uy) (z uz)) up vector3
					    (matrix*
					     (matrix! ((fx fy fz 0)
						       (ux uy uz 0)
						       (rx ry rz 0)
						       (0  0  0  1)))
					     (translate-matrix! (- x) (- y) (- z))))))))
	       for index from 1
	       for point across *vertices*
	       for new-point = (transform-vector3 point view-matrix)
	       for x = (vector3-x new-point)
	       for y = (vector3-y new-point)
	       for z = (vector3-z new-point)
	       doing (when (> x +near-plane+)
		       (let* ((projected-z (* (/ z x) *focal-length*))
			      (projected-y (* (/ y x) *focal-length*))
			      (projected-x-center (+ projected-z (/ *framebuffer-width* 2)))
			      (projected-y-center (+ projected-y (/ *framebuffer-height* 2)))
			      (screen-x  (truncate projected-x-center))
			      (screen-y  (truncate projected-y-center)))
			 ;; (pixel screen-x screen-y +pixel+)
			 (setf (aref *projected-vertices* index) ; save the projection
			       (make-vector2 :x projected-x-center :y projected-y-center))))
		     (when (<= x +near-plane+)
		       (setf (aref *projected-vertices* index) nil)))
	     (backface-cull)
	     (wireframe)))
	     ;; (rasterize)))

;; (%close-window)

;; (require 'sb-sprof)
;; (sb-sprof:with-profiling (:mode :alloc)
;;   (dotimes (i 1000)
;;     (rasterize)))
;; (sb-sprof:report)
