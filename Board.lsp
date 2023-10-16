(defun create-row (n)
  (cond 
    ((= n 1)
     '(0 A B C D E F G H I J K L M N O P Q R S))
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
            nil
        )
        (t 
        ;; print the first value of the list row with a space afterwards
        ;; t is the output stream
            
            ;;  (cond 
            ;;     ((not (equal (first row) (first '(X))))
            ;;         (cond 
            ;;             ((<= 1 (parse-integer (first row)) 9)
            ;;                     (print " ")
                            
            ;;             )

            ;;         )
            ;;     )
                
            ;; )

            (cond 
            ;; if O then show by _, but in label we have O too, so dont replace the O of label
                ((and (equal (first '(O)) (first row)) (not (equal (first '(P)) (first (rest row)))) )
                    (format t "~a " (first '(_)) )
                )
                ((and (numberp (first row)) (<= 0 (first row) 9) )
                    (format t " ~a " (first row))
                )
                (t
                    (format t "~a " (first row))

                )
            )
            (print-row (rest row))
        )
    )
)

(defun print-board (board)
  (cond
        ((null board)
            nil
        )
        (t 
           
            (print-row (first board))

            ;;  print a new line character
            (format t "~%")
            (print-board (rest board))
        )
    )
)




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
    ;; check if within bound both row and column
    (cond 
        ((not (and (<= 0 i 18) (<= 1 j 19)))
            (print "Row or column not within bounds")
            ()
        )
        (t 
            (cond 
                ;; if we reach the desired row
                ((= i 0)
                    ;; get the row 
                    ;; and call the update-row function
                    (cons 
                    (update-row (first board) j user-input) (rest board))
                )
                (t
                    (cons (first board) (update-board (rest board) (- i 1) j user-input ) )
                )
            )
        )
    )
    
)

;; (defun update-and-print-board (board i j user-input)

;;     (let* (
;;         (updated-board (update-board board i j user-input))
;;     ) 


;;     (terpri)
;;     (print-board updated-board)
;;     (terpri)

;;     )

    
;; )

;; function to ask for user input on the position
(defun ask-user-position-input ()
    (format t "Enter the position where you want to put your piece. Follow the format A10, K1, etc~%")
    (let ((user-choice (read-line)))
        ;; (format t "You entered ~a" user-choice) 
        ;; (cond 
        ;;     ((string= (string-trim " " user-choice) "")
        ;;         (ask-user-position-input )
        ;;     )
        ;; )          
        user-choice
    )

)


(defun is-numeric-char (c)
    (digit-char-p c)
)

(defun is-numeric-string (str)
  (cond
    ; Base case: an empty string is considered numeric.
    ((string= str "")
        t
    )
    ; Check the first character.
    ((is-numeric-char (char str 0))

        ; Recursively check the rest of the string.
        (is-numeric-string (subseq str 1))
    ) 
    (t 
        ()
    )
)
) ; If any character is not numeric, return false.


;; function to validate position input
;; must contain alpha and number, followng the format alpha then num
;; number must be 1<num<19
;; alphabets must be A<a<S

(defun validate-user-input (user-input)
   (let (
        (input-length (length user-input))


        )

   
    ;;  if invalid length
    (cond 
       

        ((> input-length 3)
            (print "Input Length cannot be more than 3. ")
            (terpri)
            ()
        )
        ((< input-length 2)
            (print "Input Length cannot be less than 2. ")
            (terpri)
            ()
        )
        (t 
        ;; if invalid row

            (let* 
                (
                    (col-label (char user-input 0))
                     (row-label (subseq user-input 1))
                )
           
                (cond 
                    ;; TODO check if the row input is strictly numeric      
                    ((not (is-numeric-string row-label))
                        (print "Row value must be numeric")
                        (terpri)
                        ()
                    )

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
)

;; return ((col label), (rowlabel))
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
                (append (list (+ (- (char-code col-label) (char-code #\a)) 1) (- 19 (parse-integer row-label))))
            )
            (t
                ;; assign (char-code col-label)-(char-code #\a) to i ;; convert row to numeric type 
                (append (list (+ (- (char-code col-label) (char-code #\A)) 1) (- 19 (parse-integer row-label))))
            )
        )     
    )
)
;; (validate-user-input "A10")

;; (ask-user-position-input)

;; (print (update-board '((0 1 2) (3 4 5)) 0 2 5))

;; returns the value at the specified column of the provided row
(defun get-col (row j) 
    (cond 
    
        ((= j 0)
            (first row)
        )
        (t 
            (get-col (rest row) (- j 1))
        )
    )
)

;; function to get the color at row = i and col = j
(defun get-color (board i j)
    (cond 
            ;; iboundary checking
        ((or ( < i 0) (> i 18) (< j 1) (> j 19))
            ()
        )
        ((= i 0)
            ;; get the row 
            ;; and call the update-row function
            
            (get-col (first board) j) 
        )
        (t
            (get-color (rest board) (- i 1) j ) 
        )
    )

)

;; function to check for 4 consecutive piece and return its count
(defun check-for-four (board piece-color)
   (+ (check-horizontal board piece-color 4 0) (+ (check-vertical board piece-color 4 0)  (+ (check-backward board piece-color 4 0)  (check-forward board piece-color 4 0) )))
)





; Example usage:
;; (let ((pente-board (create-board 20)))

(let ((pente-board '(
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O O O O O O O O O O W O W W W W)
     ( O O O O O B O O O O O O O O W O W W W W)
     ( O O O O O B O O O O O O O O W O W W W W)
     ( O O O O O B O O O O O O O O W O W W W W)
     ( O O O O O B O O W O O W O O W O W W W W)
     ( O O O O O B O O B O B O O O W O W W W W)
     ( O W O O O O O O B B O O O O W O W W W W)
     ( O B O O O W B B O B B W O O W O W W W W)
     ( O B O O O O O O O O O O O O W O W W W W)
     ( O O B B W O O O O O O O O O W O W W W W)
    )))
   

;; (defun sum-consecutive-col (board color count x y dx dy sum)
;; 
    ;; (print (sum-consecutive-row pente-board (first '(W)) 4 0 1 0 1 0))
    ;; (print (sum-consecutive-col pente-board (first '(W)) 4 0 1 1 0 0))
    ;; (print (sum-consecutive-forwardLower pente-board (first '(W)) 4 15 19 15 19 1 -1 0))
    ;; (print (consecutive pente-board (first '(W)) 4 15 1 1 1))
        ;; (print (sum-consecutive-backwardUpper pente-board (first '(W)) 4 0 2 0 2 1 1 0))

    ;; (print (sum-consecutive-forwardUpper pente-board (first '(W)) 4 0 18 0 18 1 -1 0))
    ;; (print pente-board)
    ;; (print (total-four-consecutive pente-board (first '(B))))
    ;; (print (get-color pente-board 18 ))
    ;; (print (capture-pair pente-board 18 1 -1 0 (first '(W)) (first '(B))))
    ;; (print (determine-capture-count pente-board 11 13 (first '(B)) (first '(W))))
    ;; (print (determine-consecutive-count pente-board 12 11 0 1 (first '(W)) 0))
    ;; (print  (five-possible pente-board 0 6 0 1 (first '(W))))
    ;; (print (five-consecutive pente-board 0 6 (first '(W))))


;; (defun save-game (board hCPair hTotScore cCPair CTotScore nextPlayer nextPlayerColor)

    ;; (save-game pente-board 0 0 0 0 'Human (first '(W)))
    ;;   (print  (check-and-capture-pairs pente-board 16 8  (first '(W)) (first '(B))))
    ;;     (print-board (check-capture-update pente-board (check-and-capture-pairs pente-board 16 8  (first '(W)) (first '(B))) 16 8  (first '(W)) (first '(B)) ))
    ;; (playgame pente-board (first '(W)) (first '(B)) (first '(B)) 1 0 0 0 0)

)