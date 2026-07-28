(in-package #:gfx)

(defun plot(function &key from to (scale-x 1) (scale-y 1))
  (loop for i from from below to
	for x = (round (* i scale-x))
	for y = (round (* (funcall function i) scale-y))
	doing
	   (%draw-pixel x y :blue)))

(defun rad->deg (radians)
  (* radians (/ 180.0 pi)))

(defun deg->rad (degrees)
  (* degrees (/ pi 180.0)))

       
(with-window 800 800 "Plot"
  (%set-target-fps 60)
  (loop until (%window-should-close)
	with camera-dx = 0
	with camera-dy = 0
	with scale = 1.0
	with rotate = 0.0
	doing
	   (with-keys ((:w :down is-w-down)
		       (:a :down is-a-down)
		       (:s :down is-s-down)
		       (:d :down is-d-down)
		       (:q :down is-q-down)
		       (:e :down is-e-down)
		       (:minus :down minus-down?)
		       (:equal :down plus-down?))
	     (with-mouse ((:left :down dragging?)
			  (:position :x x-pos :y y-pos)
			  (:delta :x delta-x :y delta-y)
			  (:wheel :move wheel-move))
	       (when is-w-down (decf camera-dy 10))
	       (when is-a-down (decf camera-dx 10))
	       (when is-s-down (incf camera-dy 10))
	       (when is-d-down (incf camera-dx 10))
	       (when is-q-down (incf rotate 2.5))
	       (when is-e-down (decf rotate 2.5))
	       (when minus-down?
		 (decf scale 0.05))
	       (when plus-down?
		 (incf scale 0.05))
	       (when dragging?
		 (decf camera-dx delta-x)
		 (decf camera-dy delta-y))
	       (when wheel-move
		 (incf scale (* 0.01 wheel-move)))
	       (with-drawing
		 (%clear-background :white)
		 (with-transform
		     (reduce #'matrix*
			     (list
			      (scale-matrix! scale scale)
			      (translate-matrix! (- camera-dx) (- camera-dy))
			      (translate-matrix! 0 800)
			      (rotate-matrix! rotate)
			      (scale-matrix! 1 -1)))
		   (draw-circle 0 0 100.0 :blue)
		   (draw-circle 0 799 100.0 :red)
		   (draw-circle 799 0 100.0 :yellow)
		   (draw-circle 799 799 100.0 :green)
		   (draw-rectangle 0 0 100 100 :pink))
		 (draw-fps 10 10)
		 (draw-text (format nil "Position delta (~a,~a)~%" camera-dx camera-dy) 10 40 25 :green)
		 (draw-text (format nil "(~a, ~a)" x-pos y-pos) (+ 20 x-pos) (- y-pos 20) 25 :green)
		 (draw-text (cond (dragging? "Dragging")
				  (is-w-down "W")
				  (is-a-down "A")
				  (is-s-down "S")
				  (is-d-down "D")
				  ((not (zerop wheel-move)) "Scrolling")
				  (t ""))
			    350	400 25 :blue))))))



