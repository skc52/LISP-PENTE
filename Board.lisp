(defun create-row (n)
  (cond 
    ((= n 1)
     '(X A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O )))))

(defun create-board (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row n) (create-board (- n 1))))))

(defun print-row (row)
  (cond 
    ((null row)
     nil)
    (t 
    ;; print the first value of the list row with a space afterwards
    ;; t is the output stream
     (format t "~a " (first row))
     (print-row (rest row)))))

(defun print-board (board)
  (cond
    ((null board)
     nil)
    (t 
     (print-row (first board))

    ;;  print a new line character
     (format t "~%")
     (print-board (rest board)))))

(let ((pente-board (create-board 20)))
  (print-board pente-board))
