(in-package #:gfx)

;; (declaim (optimize
;;           (debug 3)
;;           (speed 0)
;;           (safety 3)
;;           (space 0)))

(setf *kernel* (make-kernel 12))
(defparameter *channel* (make-channel))

(defstruct camera
  (position (make-vector3) :type vector3)
  (yaw 0.0f0 :type single-float)
  (pitch 0.0f0 :type single-float))

(defstruct basis
  forward
  right
  up)

(defstruct face
  #|
    Stores index of the vertices in *vertices* 
    Store index of vertex texture coords in *vertex-textures*
  |#
   
  (v0 0 :type fixnum)
  (v1 0 :type fixnum)
  (v2 0 :type fixnum)
  (v0-t 0 :type fixnum)
  (v1-t 0 :type fixnum)
  (v2-t 0 :type fixnum))

(defstruct projected-vertex
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (depth 1000.0f0 :type single-float)
  (u 0.0 :type single-float)
  (v 0.0 :type single-float))
  
(defparameter +camera-forward-initial+ (v! 1.0 0.0 0.0))
(defparameter +camera-right-initial+ (v! 0.0 0.0 1.0))
(defparameter +camera-position-initial+ (v! -5.0 0.0 1.0))
(defparameter +bg+ (color! 255 255 255 255))
(defparameter +fg+ (color! 0 0 0 255))
(defparameter +red+ (color! 255 0 0 255))
(defparameter +green+ (color! 0 255 0 255))
(defparameter +blue+ (color! 0 0 255 255))
(defparameter +world-up+ (v! 0.0 1.0 0.0))
(defparameter +camera-speed+ 5)
(defparameter +near-plane+ 0.1)
(defparameter +epsilon+ -1e-6)
(defparameter +pixel+ (color! 0 255 0 255))

(declaim (type fixnum *framebuffer-height*
               *framebuffer-width*))
(defparameter *framebuffer-height* 600)
(defparameter *framebuffer-width* 600)
(defparameter *vertices*
  (make-array 2000 :adjustable t :fill-pointer 0 :element-type 'vector3 :initial-element (make-vector3)))
(defparameter *projected-vertices*
  (make-array 2000 :adjustable t :fill-pointer 0 :element-type 'projected-vertex :initial-element (make-projected-vertex)))
(defparameter *vertex-textures*
  (make-array 2000 :adjustable t :fill-pointer 0 :element-type 'vector2 :initial-element (make-vector2)))
(defparameter *faces*
  (make-array 2000 :adjustable t :fill-pointer 0 :element-type 'face :initial-element (make-face)))
(defparameter *framebuffer*
  (make-array (list *framebuffer-width* *framebuffer-height*) :initial-element +bg+ :element-type 'color))
(defparameter *depthbuffer*
  (make-array (list *framebuffer-width* *framebuffer-height*)
	      :element-type 'single-float
	      :initial-element 1000.0f0))
(defparameter *camera*
  (make-camera :position +camera-position-initial+
	       :yaw 0.0
	       :pitch 0.0))
(defparameter *focal-length* 866.0)
(defparameter *mouse-sensitivity* 1)
(defparameter *camera-looking* +camera-forward-initial+)
(defparameter *backface-culled-faces*
  (make-array 2000 :adjustable t :fill-pointer 0 :element-type 'face :initial-element (make-face)))

;; load vertices & faces from file
(with-open-file (obj-stream "african_head.obj")
  (setf (fill-pointer *vertices*) 0)
  (setf (fill-pointer *faces*) 0)
  (setf (fill-pointer *vertex-textures*) 0)
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
		   ((string= "f " line :end1 2 :end2 2) 
		    (with-input-from-string (s (subseq line 2))
		      (vector-push-extend (apply #'make-face
						 (loop repeat 3
						       for v-vt-vn = (symbol-name (read s))
						       for split-by-slash = (uiop:split-string v-vt-vn :separator "/")
						       for vector-index in '(:v0 :v1 :v2)
						       for texture-index in '(:v0-t :v1-t :v2-t)
						       ;; 0 index both
						       for vector-value = (1- (parse-integer (first split-by-slash)))
						       for texture-value = (1- (parse-integer (second split-by-slash)))
						       appending (list vector-index vector-value
								       texture-index texture-value)))
					  *faces*)))
		   ((string= "vt " line :end1 3 :end2 3)
		    (with-input-from-string (s (subseq line 3))
		      (let ((x (read s))
			    (y (read s)))
			(vector-push-extend (v! x y) *vertex-textures*))))))))

;; Preallocate projected vertices
(adjust-array *projected-vertices* (length *vertices*))
(dotimes (i (length *vertices*))
  (setf (aref *projected-vertices* i)
	(make-projected-vertex)))

(defun transform-vector3(v m)
  "Transform a Vector3 by *draw-transform* (treated as w = 1)."
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14) m matrix
    (make-vector3
     :x (+ (* m0 (vector3-x v)) (* m4 (vector3-y v)) (* m8 (vector3-z v)) m12)
     :y (+ (* m1 (vector3-x v)) (* m5 (vector3-y v)) (* m9 (vector3-z v)) m13)
     :z (+ (* m2 (vector3-x v)) (* m6 (vector3-y v)) (* m10 (vector3-z v)) m14))))

(defun transform-vector3f(v m)
  "Transform a Vector3 in-place by m (treated as w = 1)."
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14) m matrix
    (let* ((vx (vector3-x v))
           (vy (vector3-y v))
           (vz (vector3-z v))
           (xp (+ (* m0 vx) (* m4 vy) (* m8 vz) m12))
           (yp (+ (* m1 vx) (* m5 vy) (* m9 vz) m13))
           (zp (+ (* m2 vx) (* m6 vy) (* m10 vz) m14)))
      (with-members ((x vx) (y vy) (z vz)) v vector3
        (setf vx xp vy yp vz zp))
      v)))

(defun transform-vector3-into(dest v m)
  "Transform a Vector3 by m into dest (treated as w = 1)."
  (vector-copyf dest v)
  (transform-vector3f dest m)
  dest)

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
    (%set-target-fps 60)
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
			      (forward-scaled (vector-mul (* +camera-speed+ dt)
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
			      (right-scaled (vector-mul (* +camera-speed+ dt)
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
			      (up-scaled (vector-mul (* +camera-speed+ dt)
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
		     (%draw-texture (render-texture-texture framebuffer)
				    0
				    0
				    :white)
		     (%draw-fps 10 10)
		     (with-members (yaw pitch) *camera* camera
		       (%draw-text (format nil "Yaw ~a~%Pitch ~a" yaw pitch) 10 80 20 :blue)))))))))

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
			  (let* ((v1 (aref *vertices* v0-index))
				 (v2 (aref *vertices* v1-index))
				 (v3 (aref *vertices* v2-index))
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
		      (let* ((v1 (aref *vertices* v0-index))
			     (v2 (aref *vertices* v1-index))
			     (v3 (aref *vertices* v2-index)))
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

(declaim
 (ftype (function (projected-vertex projected-vertex)
                  (values single-float single-float single-float))
        edge-function-coefficients))

(defun edge-function-coefficients(v0 v1)
  "Takes 2 vector2 and returns the multiple value(edge-function dx dy)"
  
  (with-members ((x x0) (y y0)) v0 projected-vertex
    (with-members ((x x1) (y y1)) v1 projected-vertex
      (let ((a (- y0 y1))
	    (b (- x1 x0))
	    (c (- (* x0 y1) (* x1 y0))))
	(values a b c)))))

(defun hash32 (x)
  (let ((x (logxor x (ash x -16))))
    (setf x (* x #x45d9f3b))
    (setf x (logxor x (ash x -16)))
    (setf x (* x #x45d9f3b))
    (logxor x (ash x -16))))

(defun random-color (seed)
  (let ((h (hash32 seed)))
    (color!
     (ldb (byte 8  0) h)
     (ldb (byte 8  8) h)
     (ldb (byte 8 16) h)
     255)))

(defun rasterize()
  (with-image (model-image "african_head_diffuse.png")
    (with-image-colors (model-colors model-image)
  (loop for f across *backface-culled-faces*
	for face-index of-type fixnum from 0
	doing
	   (with-members ((v0 v0-index) (v1 v1-index) (v2 v2-index) v0-t v1-t v2-t) f face
	     (let* ((v1 (aref *projected-vertices* v0-index))
		    (v2 (aref *projected-vertices* v1-index))
		    (v3 (aref *projected-vertices* v2-index))
		    (v0-texture (aref *vertex-textures* v0-t))
		    (v1-texture (aref *vertex-textures* v1-t))
		    (v2-texture (aref *vertex-textures* v2-t)))
	       (and v1 v2 v3
		    (with-members ((x v1-x) (y v1-y) (depth v1-depth)) v1 projected-vertex
		      (with-members ((x v2-x) (y v2-y) (depth v2-depth)) v2 projected-vertex
			(with-members ((x v3-x) (y v3-y) (depth v3-depth)) v3 projected-vertex
			  ;; bounding box
 			  (let* ((top (ceiling (max v1-y v2-y v3-y)))
				 (bottom (floor (min v1-y v2-y v3-y)))
				 (left (floor (min v1-x v2-x v3-x)))
				 (right (ceiling (max v1-x v2-x v3-x))))
			    (declare (type fixnum top bottom left right))
			    (minf top (1- *framebuffer-height*))
			    (minf right (1- *framebuffer-width*))
			    (maxf bottom 0)
			    (maxf left 0)
			    ;; get the edge function coefficients for each edge
			    (multiple-value-bind (v1-to-v2-a v1-to-v2-b v1-to-v2-c)
				(edge-function-coefficients v1 v2)
			      (multiple-value-bind (v2-to-v3-a v2-to-v3-b v2-to-v3-c)
				  (edge-function-coefficients v2 v3)
				(multiple-value-bind (v3-to-v1-a v3-to-v1-b v3-to-v1-c)
				    (edge-function-coefficients v3 v1)
				  (let ((initial-v1-to-v2 (+ (* v1-to-v2-a left) (* v1-to-v2-b bottom) v1-to-v2-c))
					(initial-v2-to-v3 (+ (* v2-to-v3-a left) (* v2-to-v3-b bottom) v2-to-v3-c))
					(initial-v3-to-v1 (+ (* v3-to-v1-a left) (* v3-to-v1-b bottom) v3-to-v1-c)))
				    (let* ((triangle-area (+ (* v1-to-v2-a v3-x) (* v1-to-v2-b v3-y) v1-to-v2-c)))
				      (loop for py from bottom to top
					    with row-v1-to-v2 = initial-v1-to-v2
					    with row-v2-to-v3 = initial-v2-to-v3
					    with row-v3-to-v1 = initial-v3-to-v1
					    do
					       (loop for px from left to right
						     with captured-row-v1-to-v2 = row-v1-to-v2
						     with captured-row-v2-to-v3 = row-v2-to-v3
						     with captured-row-v3-to-v1 = row-v3-to-v1
						     when (and (> captured-row-v1-to-v2 0.0f0)
							       (> captured-row-v2-to-v3 0.0f0)
							       (> captured-row-v3-to-v1 0.0f0))
						       do
							  (with-members ((x u0) (y v0)) v0-texture vector2
							    (with-members ((x u1) (y v1)) v1-texture vector2
							      (with-members ((x u2) (y v2)) v2-texture vector2
							  (let* ((v1-weight (/ captured-row-v2-to-v3 triangle-area))
								 (v2-weight (/ captured-row-v3-to-v1 triangle-area))
								 (v3-weight (/ captured-row-v1-to-v2 triangle-area))
								 (pu (+ (* v1-weight u0) (* v2-weight u1) (* v3-weight u2)))
								 (pv (+ (* v1-weight v0) (* v2-weight v1) (* v3-weight v2)))
								 (current-depth (+ (* v1-weight v1-depth)
										   (* v2-weight v2-depth)
										   (* v3-weight v3-depth)))
								 (old-depth (aref *depthbuffer* px py)))
							    (declare (single-float
								      v1-weight
								      v2-weight
								      v3-weight
								      current-depth
								      old-depth))
							    (when (< current-depth old-depth)
							      (setf (aref *depthbuffer* px py) current-depth)
							      (pixel px py (sample-texture pu pv model-image model-colors)))))))
						     doing
							(incf captured-row-v1-to-v2 v1-to-v2-A)
							(incf captured-row-v2-to-v3 v2-to-v3-A)
							(incf captured-row-v3-to-v1 v3-to-v1-A))
					       (incf row-v1-to-v2 v1-to-v2-B)
					       (incf row-v2-to-v3 v2-to-v3-B)
					       (incf row-v3-to-v1 v3-to-v1-B))))))))))))))))))

(defun sample-texture(u v image colors)
  (let* ((tx (floor (* u (1- (image-width image)))))
	 (ty (floor (* (- 1.0 v) (1- (image-height image)))))
	 (index (+ tx (* ty (image-width image)))))
    (mem-aref colors '(:struct %color) index)))


(defun clear-depthbuffer()
  (dotimes (y *framebuffer-height*)
    (dotimes (x *framebuffer-width*)
      (setf (aref *depthbuffer* x y) 1000.0f0))))

(display #'(lambda()
	     (loop
	       initially (clear-framebuffer)
		 initially (clear-depthbuffer)
 		 initially (adjust-array *projected-vertices* (length *vertices*))
 		 initially (setf (fill-pointer *projected-vertices*) (length *vertices*))
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
	       with new-point = (make-vector3)
	       for index from 0
	       for point across *vertices*
	       doing
		  (transform-vector3-into new-point point view-matrix)
	       doing
		  (with-members (x y z) new-point vector3
		    (when (> x +near-plane+)
		      (let* ((projected-z (* (/ z x) *focal-length*))
			     (projected-y (* (/ y x) *focal-length*))
			     (projected-x-center (+ projected-z (/ *framebuffer-width* 2)))
			     (projected-y-center (+ projected-y (/ *framebuffer-height* 2))))
			;; (screen-x  (truncate projected-x-center))
			;; (screen-y  (truncate projected-y-center)))
			;; (pixel screen-x screen-y +pixel+)
			(with-members ((x pjx) (y pjy) depth) (aref *projected-vertices* index) projected-vertex ; save the projection
			  (setf pjx projected-x-center
				pjy projected-y-center
				depth x))))
		    (when (<= x +near-plane+)
		      (setf (aref *projected-vertices* index) nil))))
	     (backface-cull)
	     ;; (wireframe)))
	     (rasterize)))

;; (%close-window)

;; (require 'sb-sprof)
;; (sb-sprof:with-profiling (:mode :alloc)
;;   (dotimes (i 1000)
;;     (rasterize)))
;; (sb-sprof:report)
