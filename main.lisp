(+ 1 2)

(defun hello-world ()
    (format t "Hello, World!"))

(hello-world)

(list 1 2 3)
'(1 2 3)

(list :a 1 :b 2 :c 3)
(getf (list :a 1 :b 2 :c 3) :a)


(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))

(if t 'hello)

(equal 1.0 1.0d0)
':hello
(keywordp :hello)
(keywordp 'hello)
"Hello, World!"
(gensym)

(apropos "sqrt")
(sqrt 10)
(isqrt 10)

(defvar *primes* ())
(loop for i from 1 to 10 while (< i 7) collecting i)
(loop for x in '(1 2 3) collect x)
(eql :hello :hello)

(defun next-prime(&optional (x 2))
    (loop for prime in *primes*
        while (<= prime (isqrt x))
        never (zerop (mod x prime))
        ))
(next-prime 77)
