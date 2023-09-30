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

;; (let ((pente-board (create-board 20)))
;;   (print-board pente-board))


;; write function to set piece to a location
;; will return the updated row
;; here row is the entire row and not indexing, and i is the indexing for column
(defun update-row (row i user-input)
    (cond 
        ((= i 0)
            (cons user-input (rest row))
        )

        (t
            (cons (first row) (update-row (rest row) (- i 1) user-input) )
        )
    )
)

(defun update-board (board i j user-input)
    (cond 
        ;; if we reach the desired row
        ((= i 0)
            ;; get the row 
            ;; and call the update-row function
            (cons 
            (update-row (first board) j user-input) (rest board))
        )
        (t
            (const (first board) (update-board (rest board) (- i 1) j user-input ) )
        )
    )
)

(print (update-board '((0 1 2) (3 4 5)) 0 2 5))