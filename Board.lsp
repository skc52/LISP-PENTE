
;; /* *********************************************************************
;;    Function Name: create-row
;;    Purpose: To create a row for a game board
;;    Parameters:
;;        n, an integer representing the row number
;;    Return Value: A list representing a game board row
;;    Algorithm:
;;        1) If n is 1, create a row with labels A to S and initial values of 'O'
;;        2) Otherwise, create a row with the row number and initial values of 'O'
;;    Assistance Received: none
;; ********************************************************************* */
(defun create-row (n)
  (cond 
    ((= n 1)
     '(0 A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O 'O ))))
)


;; /* *********************************************************************
;;    Function Name: create-board
;;    Purpose: To create a game board for a class
;;    Parameters:
;;        n, an integer representing the number of rows in the board
;;    Return Value: A list representing a game board with multiple rows
;;    Algorithm:
;;        1) If n is 0, return nil (an empty board)
;;        2) Otherwise, create a row and append it to the rest of the board
;;    Assistance Received: none
;; ********************************************************************* */
(defun create-board (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row n) (create-board (- n 1)))))
)


;; /* *********************************************************************
;;    Function Name: print-row
;;    Purpose: To print a row of the game board with special formatting
;;    Parameters:
;;        row, a list representing a row of the game board
;;    Return Value: nil (printing to console)
;;    Algorithm:
;;        1) If the row is empty (null), return nil (no further action)
;;        2) For each element in the row:
;;             a) If it's 'O' and not part of a label, print it as '_'
;;             b) If it's a number between 0 and 9, print it with leading spaces
;;             c) Otherwise, print it as is
;;        3) Recursively call the function for the rest of the row
;;    Assistance Received: none
;; ********************************************************************* */
(defun print-row (row)
  (cond 
        ((null row)
            nil
        )
        (t 
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


;; /* *********************************************************************
;;    Function Name: print-board
;;    Purpose: To print the entire game board with special formatting
;;    Parameters:
;;        board, a list representing the game board with multiple rows
;;    Return Value: nil (printing to console)
;;    Algorithm:
;;        1) If the board is empty (null), return nil (no further action)
;;        2) For each row in the board:
;;             a) Call the print-row function to print the row
;;             b) Print a new line character to move to the next row
;;        3) Recursively call the function for the rest of the board
;;    Assistance Received: none
;; ********************************************************************* */
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


;; /* *********************************************************************
;;    Function Name: update-row
;;    Purpose: To update a specific column in a row of the game board
;;    Parameters:
;;        row, a list representing a row of the game board
;;        i, an integer representing the column index to update
;;        user-input, the value to set in the specified column
;;    Return Value: A list representing the updated row
;;    Algorithm:
;;        1) If i is 0, update the first element of the row with user-input
;;        2) Otherwise, recursively call the function for the rest of the row
;;    Assistance Received: none
;; ********************************************************************* */
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


;; /* *********************************************************************
;;    Function Name: update-board
;;    Purpose: To update a specific cell on the game board
;;    Parameters:
;;        board, a list representing the game board with multiple rows
;;        i, an integer representing the row index
;;        j, an integer representing the column index
;;        user-input, the value to set in the specified cell
;;    Return Value: A list representing the updated game board
;;    Algorithm:
;;        1) Check if i and j are within the bounds of the board
;;        2) If i is 0, update the specified cell in the first row
;;        3) Otherwise, recursively call the function for the rest of the board
;;    Assistance Received: none
;; ********************************************************************* */
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


;; /* *********************************************************************
;;    Function Name: ask-user-position-input
;;    Purpose: To prompt the user to enter a position for their game piece
;;    Parameters: None
;;    Return Value: A string representing the user's input (position)
;;    Algorithm:
;;        1) Display a message to instruct the user on the input format
;;        2) Read the user's input as a string and store it in 'user-choice'
;;        3) Return the user's input as a string
;;    Assistance Received: none
;; ********************************************************************* */
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

;; /* *********************************************************************
;;    Function Name: is-numeric-char
;;    Purpose: To check if a character is a numeric digit
;;    Parameters:
;;        c, a character to be checked
;;    Return Value: True if the character is a numeric digit, false otherwise
;;    Algorithm:
;;        1) Use the 'digit-char-p' function to determine if 'c' is a numeric digit
;;    Assistance Received: none
;; ********************************************************************* */
(defun is-numeric-char (c)
    (digit-char-p c)
)

;; /* *********************************************************************
;;    Function Name: is-numeric-string
;;    Purpose: To check if a string contains only numeric characters
;;    Parameters:
;;        str, a string to be checked
;;    Return Value: True if the string contains only numeric characters, false otherwise
;;    Algorithm:
;;        1) Base case: An empty string is considered numeric.
;;        2) Check the first character in the string.
;;        3) Recursively check the rest of the string.
;;    Assistance Received: none
;; ********************************************************************* */
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
)

;; /* *********************************************************************
;;    Function Name: validate-user-input
;;    Purpose: To validate user input for a game piece position
;;    Parameters:
;;        user-input, a string representing the user's input
;;    Return Value: None (prints error messages if validation fails)
;;    Algorithm:
;;        1) Check if the input length is invalid (must be 2 or 3 characters).
;;        2) Extract the column label (first character) and row label (remaining characters).
;;        3) Validate the row label (must be strictly numeric, 1 <= num <= 19).
;;        4) Validate the column label (must be an uppercase letter 'A' to 'S' or lowercase letter 'a' to 's').
;;    Assistance Received: none
;; ********************************************************************* */
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
            (let* 
                (
                    (col-label (char user-input 0))
                     (row-label (subseq user-input 1))
                )
           
                (cond 
                    ;;  check if the row input is strictly numeric      
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

;; /* *********************************************************************
;;    Function Name: parse-position
;;    Purpose: To parse user input representing a game piece position into column and row indices
;;    Parameters:
;;        position-input, a string representing the user's input (e.g., "A10")
;;    Return Value: A list containing column index and row index as integers (e.g., (col-label row-label))
;;    Algorithm:
;;        1) Extract the column label and row label from the input.
;;        2) Determine the column index (i) based on the column label (A to S or a to s).
;;        3) Determine the row index (j) by converting the row label to an integer and subtracting it from 19.
;;        4) Return a list containing (col-label row-label) as integers.
;;    Assistance Received: none
;; ********************************************************************* */
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


;; /* *********************************************************************
;;    Function Name: get-col
;;    Purpose: To retrieve the value (character) at the specified column of the provided row
;;    Parameters:
;;        row, a list representing a row of the game board
;;        j, an integer representing the column index
;;    Return Value: The value (character) at the specified column of the row
;;    Algorithm:
;;        1) Recursively traverse the row to find the value at the specified column index.
;;    Assistance Received: none
;; ********************************************************************* */
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

;; /* *********************************************************************
;;    Function Name: get-color
;;    Purpose: To retrieve the color of a game piece at the specified position on the board
;;    Parameters:
;;        board, a nested list representing the game board
;;        i, an integer representing the row index
;;        j, an integer representing the column index
;;    Return Value: The color of the piece at the specified position, which can be 'O' (white), 'B' (black), or NIL (empty)
;;    Algorithm:
;;        1) Check if the provided indices (i, j) are within the bounds of the board.
;;        2) If not within bounds, return NIL to indicate an invalid position.
;;        3) If within bounds, determine the color by accessing the corresponding cell on the board.
;;    Assistance Received: none
;; ********************************************************************* */
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
