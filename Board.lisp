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

;; function to ask for user input on the position
(defun ask-user-position-input ()
    (format t "Enter the position where you want to put your piece. Follow the format A10, K1, etc~%")
    (let ((user-choice (read-line)))
        (format t "You entered ~a" user-choice)           
    

        ;; check if input position is valid
        (cond 
            ((not (validate-user-input user-choice))
                (ask-user-position-input)
            )
        )
    )

)

;; function to validate position input
;; must contain alpha and number, followng the format alpha then num
;; number must be 1<num<19
;; alphabets must be A<a<S

(defun validate-user-input (user-input)
   (let (
        (input-length (length user-input))
        (col-label (char user-input 0))
        (row-label (subseq user-input 1)))

   
    ;;  if invalid length
    (cond 
        ((> input-length 3)
            (print "Input Length cannot be more than 3. ")
            (terpri)
            ()
        )
        
        (t 
        ;; if invalid row
            (cond 
                ;; TODO check if the row input is strictly numeric
                ;; Col input validation is fine


                ((not (<= 1 (parse-integer row-label) 19))
                    (print "Invalid row input.")
                    (terpri)
                    ;; return nil
                    ()
                )
                (t 
                    ;; if invalid column
                    (cond 
                        ((not (or (<= (char-code #\a) (char-code col-label)  (char-code #\s)) (<= (char-code #\A) (char-code col-label)  (char-code #\S)) ))
                            (print "Invalid Column Input.")
                            (terpri)
                            ;; return nil
                            ()
                        )

                        (t 
                            t
                        )  
                    
                    )

                )
            )

        )
    )
    
   )    
)


(defun parse-position (position-input)
    ;; determine row label and column label
    ;; convert both to indexing i and j
    (let (
        (col-label (char position-input 0))
        (row-label (subseq position-input 1))
        
        )   

        ;; if col in between A to S
        ;; if col in between a to s

        (cond 
            ((<= (char-code #\a) (char-code col-label)  (char-code #\s))
                ;; assign (char-code col-label)-(char-code #\a) to i ;; convert row to numeric type 
                (append (list (+ (- (char-code col-label) (char-code #\a)) 1)) (list (parse-integer row-label)))
            )
            (t
                ;; assign (char-code col-label)-(char-code #\a) to i ;; convert row to numeric type 
                (append (list (+ (- (char-code col-label) (char-code #\A)) 1)) (list (parse-integer row-label)))
            )
        )     
    )
)

;; (validate-user-input "A10")

;; (ask-user-position-input)

;; (print (update-board '((0 1 2) (3 4 5)) 0 2 5))