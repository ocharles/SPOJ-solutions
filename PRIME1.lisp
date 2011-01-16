(defun sieve (max)
  (let* ((size (floor max 2))
         (sieve (make-array size
                            :element-type 'bit
                            :initial-element 0)))
    (loop for i from 3 upto (isqrt max) by 2
       for index = (floor i 2)
       when (= 0 (bit sieve index))
       do (loop for j from (+ index i) below size by i
             do (setf (bit sieve j) 1)))
    (loop for i from 3 below max by 2
       when (= 0 (bit sieve (floor i 2)))
       collect i)))

(defvar *composites* (sieve (isqrt 1000000000)))

(defun primes-between (min max)
  (let* ((size (1+ (- max min)))
         (sieve (make-array size
                            :element-type 'bit
                            :initial-element 0)))
    (labels ((number->index (n) (- n min))
             (index->number (i) (+ i min))
             (first-index (n)
               (number->index (max (+ n n)
                                   (* n (ceiling min n))))))
      (loop for composite in (cons 2 *composites*)
         when (< composite max)
         do (loop for i from (first-index composite) below size by composite
               do (setf (bit sieve i) 1)))
      (loop for i from (max 2 min) upto max
         when (= (bit sieve (number->index i)) 0)
         collect i))))

(dotimes (test-number (parse-integer (read-line)))
  (let* ((spec (read-line))
         (separator (position #\Space spec))
         (min (parse-integer (subseq spec 0 separator)))
         (max (parse-integer (subseq spec (1+ separator)))))
    (format t "~{~a~%~}~%" (primes-between min max))))
