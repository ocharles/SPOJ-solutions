(defun letterp (c)
  (or (char-equal c #\a)
      (char-equal c #\z)
      (char-lessp #\a c #\z)))

(defun operatorp (c)
  (or (char= c #\+)
      (char= c #\-)
      (char= c #\*)
      (char= c #\/)
      (char= c #\^)))

(defun format-infix (str)
  (let ((operators '()))
    (flet
      ((process-char (c)
         (cond
           ((letterp c) (format t "~a" c))
           ((operatorp c) (push c operators))
           ((char= c #\() (push c operators))
           ((char= c #\))
            (loop for operator = (pop operators)
               then (pop operators)
               until (char= operator #\()
               do (format t "~a" operator))))))
    (map nil #'process-char str)
    (loop for operator in operators
       do (format t "~a" operator))
    (terpri))))

(dotimes (test-number (read))
  (format-infix (read-line)))
