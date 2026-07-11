
(format t "~:[FAIL~;PASS~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
(= 1 1.0)
#(1 2 3)
(make-array 5 :fill-pointer 0 :initial-element nil)

(loop for i from 0 to 10 collect i)
(loop for i upfrom 0 to 10 collect i)
(loop for i downfrom 10 to 0 collect i)

(loop for i from 0 to 10 collect i)
(loop for i from 0 upto 10 collect i)
(loop for i from 0 below 10 collect i) ; invalid
(loop for i from 20 above 10 collect i)
(loop for i from 20 below 10 collect i) ; invalid

(loop for i upto 10 collect i)
(loop for i from 0 downto -10 collect i)
(loop for i from 0 downto -10 collect i)

(loop repeat 10
    for x = 0 then y
    and y = 1 then (+ x y)
    collect x)

(loop for (item . rest) on '(1 2 3 4 5)
    do (format t "~a" item)
    when rest do (format t ", "))

(loop repeat 10 collect (random 10000))

(loop for i in (loop repeat 100 collect (random 10000))
    counting (evenp i) into evens
    counting (oddp i) into odds
    summing i into total
    maximizing i into max
    minimizing i into min
    finally (return (list min max total evens odds)))
