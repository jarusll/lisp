(in-package #:gfx)

(declaim (optimize
          (debug 3)
          (speed 0)
          (safety 3)
          (space 0)))

(defstruct camera
  position
  looking)

(defparameter *bg* (color!))
(defparameter *fg* (color! 0 0 0))
(defparameter *debug-points* nil)
(defparameter *framebuffer-height* 500)
(defparameter *framebuffer-width* 500)
(defparameter *vertices* (make-array 2000 :adjustable t :fill-pointer 0))
(defparameter *faces* (make-array 2000 :adjustable t :fill-pointer 0))
(defparameter *framebuffer* (make-array (list *framebuffer-width* *framebuffer-height*) :initial-element *bg*))
(defparameter *camera* (make-camera :position (v! -5.0 0.0 0.0)
				       :looking (v! 1.0 0.0 0.0)))
(defparameter *focal-length* 866.0)

;; load vertices from file
(with-open-file (obj-stream "african_head.obj")
  (setf (fill-pointer *vertices*) 0)
  (setf (fill-pointer *faces*) 0)
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
		      (vector-push-extend (apply #'make-vector3
						 (loop repeat 3
						       for v-vt-vn = (symbol-name (read s))
						       for axis in '(:x :y :z)
						       appending
						       (list
							axis
							(parse-integer v-vt-vn
								       :junk-allowed t))))
					  *faces*)))))))

(defun pixel(x y color)
  (setf (aref *framebuffer* x y) color))

(defun display(painter)
  (with-window *framebuffer-width* *framebuffer-height* "Framebuffer"
    (%set-target-fps 60)
    (with-texture framebuffer *framebuffer-width* *framebuffer-height*
      (labels ((repaint()
		 (funcall painter)
		 (with-texture-mode framebuffer
		   (%clear-background :white)
		   (dotimes (x-index *framebuffer-width*)
		     (dotimes (y-index *framebuffer-height*)
		       (let ((c (aref *framebuffer* x-index y-index)))
			 (draw-pixel x-index y-index c)))))))
	(repaint)
	(loop until (%window-should-close) doing
	  (with-keys ((:w :down forward)
		      (:a :down left)
		      (:s :down backward)
		      (:d :down right)
		      (:left_control :down down)
		      (:space :down up))
	    (with-members (position) *camera* camera
	      (when forward
		(incf (vector3-x position) 0.25))
	      (when backward
		(decf (vector3-x position) 0.25))
	      (when up
		(incf (vector3-y position) 0.25))
	      (when down	   
		(decf (vector3-y position) 0.25))
	      (when right
		(incf (vector3-z position) 0.25))
	      (when left	   
		(decf (vector3-z position) 0.25))
	      (when (or forward backward left right up down)
		(repaint)))
	    (with-drawing
	      (%clear-background :white)
	      (with-math-coordinates (0 *framebuffer-height*)
		(%draw-texture (render-texture-texture framebuffer)
			       0
			       0
			       :white))
	      (draw-fps 10 10))))))))

(display #'(lambda()
	     (loop
	       initially (dotimes (x *framebuffer-width*)
			   (dotimes (y *framebuffer-height*)
			     (setf (aref *framebuffer* x y) *bg*)))
;		 initially (setf *debug-points* nil)
	       for point across *vertices*
	       for new-point = (with-members
				   (x y z) (camera-position *camera*) vector3
				 (transform-vector-3 point (translate-matrix! (- x) (- y) (- z))))
	       for x = (vector3-x new-point)
	       for y = (vector3-y new-point)
	       for z = (vector3-z new-point)
	       for projected-z = (* (/ z x) *focal-length*)
	       for projected-y = (* (/ y x) *focal-length*)
	       for screen-x = (truncate (+ projected-z (/ *framebuffer-width* 2)))
	       for screen-y = (truncate (+ projected-y (/ *framebuffer-height* 2)))
	       doing
		  (when (and (<= 0 screen-x (1- *framebuffer-width*))
			     (<= 0 screen-y (1- *framebuffer-height*)))
;		    (push (list projected-z projected-y) *debug-points*)
		    (pixel screen-x screen-y *fg*)))))
;	       finally
;		  (print (length *debug-points*)))))

	 

(%close-window)

(defun transform-vector-3 (v m)
  "Transform a Vector3 by *draw-transform* (treated as w = 1)."
  (with-members (m0 m4 m8 m12 m1 m5 m9 m13 m2 m6 m10 m14) m matrix
    (make-vector3
     :x (+ (* m0 (vector3-x v)) (* m4 (vector3-y v)) (* m8 (vector3-z v)) m12)
     :y (+ (* m1 (vector3-x v)) (* m5 (vector3-y v)) (* m9 (vector3-z v)) m13)
     :z (+ (* m2 (vector3-x v)) (* m6 (vector3-y v)) (* m10 (vector3-z v)) m14))))


