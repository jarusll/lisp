(in-package #:gfx)

(defparameter *framebuffer-height* 400)
(defparameter *framebuffer-width* 400)
(defparameter *points* (make-array 10000 :adjustable t :fill-pointer 0))
(defparameter *framebuffer* (make-array (list *framebuffer-width* *framebuffer-height*) :initial-element #(255 255 255)))

(defun pixel(x y color)
  (destructuring-bind (r g b) color
    (setf (aref *framebuffer* x y) (vector r g b))))

(defun display(painter)
  (with-window *framebuffer-width* *framebuffer-height* "Framebuffer"
    (%set-target-fps 10)
    (with-texture framebuffer *framebuffer-width* *framebuffer-height*
      (loop until (%window-should-close) doing
	(funcall painter)
	(with-texture-mode framebuffer
	  (%clear-background :white)
	  (dotimes (x-index *framebuffer-width*)
	    (dotimes (y-index *framebuffer-height*)
	      (let* ((item (aref *framebuffer* x-index y-index))
		     (color (color! (aref item 0)
				(aref item 1)
				(aref item 2))))
		(draw-pixel x-index y-index color)))))
	(with-drawing
	  (with-math-coordinates (0 *framebuffer-height*)
	    (%draw-texture (render-texture-texture framebuffer)
			   0
			   0
			   :white))
	  (draw-rectangle-v (v! 0 0) (v! 90 40) (color! 255 255 255 127)) ;fps bg
	  (draw-fps 10 10))))))
	  

(display #'(lambda()
	     (dotimes (i 400)
	       (dotimes (j 400)
		 (pixel i j (loop repeat 3 collecting (random 255)))))))

;; load vertices from file
(with-open-file (obj-stream "african_head.obj")
  (setf (fill-pointer *points*) 0)
  (loop for line = (read-line obj-stream nil)
	while (and line (<= 2 (length line)))
	doing
	   (when (string= "v " line :end2 2)
	     (with-input-from-string (s (subseq line 2))
	       (let ((x (read s))
		     (y (read s))
		     (z (read s)))
		 (vector-push-extend (vector x y z) *points*))))))


(%close-window)
