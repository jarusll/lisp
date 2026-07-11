(declaim (optimize (speed 3) (safety 0) (debug 0)))

(ql:quickload 'lparallel)
(setf lparallel:*kernel* (lparallel:make-kernel 12))

(defun primep(x)
    (declare (type (unsigned-byte 32) x))
    (and (> x 1)
        (loop for i from 2 to (isqrt x)
            always (not (zerop (mod x i))))))

(defun prime-range(start end)
    (declare (type (unsigned-byte 32) start end))
    (loop for n from start to end if (primep n) collect n))

(prime-range 1 100)

(defun split-range (start end parts)
    (declare (type (unsigned-byte 32) start end parts))
    (let* ((count (1+ (- end start)))
        (chunk-size (ceiling count parts)))
    (loop for chunk-start from start to end by chunk-size
        collect (list chunk-start
                        (min end (+ chunk-start chunk-size -1))))))

(time
    (lparallel:pmap 'list
        (lambda (range)
            (destructuring-bind (start end) range
            (prime-range start end)))
        (split-range 1 1000000000 12)))

(time (loop for i from 1 to 1000000 if (primep i) collect i))
(lparallel:pmap 'list #'primep (loop for x from 1 to 1000000 collect x))
(time (lparallel:premove-if-not #'primep (loop for x from 1 to 1000000 collect x)))

(primep 10)
(primep 7)

(defun benchmark-n (n thunk)
    (declare (type (unsigned-byte 32) n))
    (declare (type function thunk))
    (progn
        (dotimes (_ 10)
            (funcall thunk))
        (sb-ext:gc :full t)
        (loop repeat n
            collect (let ((start (get-internal-real-time)))
                (funcall thunk)
                (* 1000.0
                    (/ (float (- (get-internal-real-time) start))
                            (float internal-time-units-per-second)))))))

(defun compare()
    (let ((times (benchmark-n 100
        #'(lambda()
            (time
                (lparallel:pmap 'list
                    (lambda (range)
                        (destructuring-bind (start end) range
                        (prime-range start end)))
                    (split-range 1 10000000 12)))
            ))))
        (/ (reduce #'+ times)
            (float (length times) 1.0d0))))


