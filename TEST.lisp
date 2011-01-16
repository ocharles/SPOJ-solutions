(loop for input = (read-line) then (read-line)
   while (not (equal input "42"))
   do (format t "~a~%" input))
