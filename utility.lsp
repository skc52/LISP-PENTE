

(load "Board.lsp")

;; this file will have all utility functions


;; /* *********************************************************************
;;    Function Name: consecutive
;;    Purpose: To check if it's possible to form a consecutive sequence of pieces of a given color on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        x, an integer representing the starting row index
;;        y, an integer representing the starting column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;    Return Value: True (T) if it's possible to form a consecutive sequence; otherwise, Nil
;;    Algorithm:
;;        1) Recursively traverse the board in the specified direction (dx, dy) from the starting position (x, y).
;;        2) Check if a piece of the given color is found at each step.
;;        3) If the desired count is reached, return True (T); otherwise, continue the search.
;;    Assistance Received: none
;; ********************************************************************* */
(defun consecutive (board color count x y dx dy)
   (cond
        ((= count 0)
            t   
        )
        (t
            (cond
                ((not (and (<= 0 x 18) (<= 1 y 19)))
                    
                    ()
                )
                (t
                    (cond 
                        ((equal (get-color board x y) color)
                            (consecutive board color (- count 1) (+ x dx) (+ y dy) dx dy)
                        )
                        (t 
                            ()
                        )
                    )
                )            
            )            
        )        
    )
)


;; /* *********************************************************************
;;    Function Name: sum-consecutive-row
;;    Purpose: To calculate the total number of consecutive sequences of pieces of a given color in a specific row of the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        x, an integer representing the current row index
;;        y, an integer representing the current column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        sum, an integer representing the current total of consecutive sequences
;;    Return Value: The total number of consecutive sequences in the specified row.
;;    Algorithm:
;;        1) Recursively traverse the row, checking for consecutive sequences.
;;        2) If a consecutive sequence is found, increment the sum.
;;        3) Continue the search until the entire row has been examined.
;;    Assistance Received: none
;; ********************************************************************* */
(defun sum-consecutive-row (board color count x y dx dy sum)
    (cond 
 
        ((> x 18)
        ;; return the sum
            sum

        )
        (t 
            (cond 
                ((> y 16)
                    ;; start looking at new row
                    (sum-consecutive-row board color count (+ x 1) 1 dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and y by 4
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-row board color count x (+ y 4) dx dy (+ sum 1))
                        )
                        ;; else increment y by 1 and continue looking
                        (t
                            (sum-consecutive-row board color count x (+ y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)


;; /* *********************************************************************
;;    Function Name: sum-consecutive-col
;;    Purpose: To calculate the total number of consecutive sequences of pieces of a given color in a specific column of the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        x, an integer representing the current row index
;;        y, an integer representing the current column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        sum, an integer representing the current total of consecutive sequences
;;    Return Value: The total number of consecutive sequences in the specified column.
;;    Algorithm:
;;        1) Recursively traverse the column, checking for consecutive sequences.
;;        2) If a consecutive sequence is found, increment the sum.
;;        3) Continue the search until the entire column has been examined.
;;    Assistance Received: none
;; ********************************************************************* *
(defun sum-consecutive-col (board color count x y dx dy sum)
    (cond 
 
        ((> y 19)
        ;; return the sum
            sum

        )
        (t 
            (cond 
                ((> x 15)
                    ;; start looking at new col
                    (sum-consecutive-col board color count 0 (+ y 1) dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-col board color count (+ x 4) y dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and continue looking
                        (t
                            (sum-consecutive-col board color count (+ x 1) y dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)




;; backward Lower meaning the diagonal starts at 1 col meaning the lower part of the board
;; it is not really the lower part of the board as the actual lower part would be x = 18 but 
;; it is lower compared to the x = 0
;; backward upper would mean the diagonal start st 0 row meaning the upper part of the board 
;; ox and oy represent the starting position for each diagonal, their origin indices
;; once a diagonal is scanned through, then these values are changed representing the diagonal next to them
;; whose turn is now to be scanned


;; /* *********************************************************************
;;    Function Name: sum-consecutive-backwardLower
;;    Purpose: To calculate the total number of consecutive sequences of pieces of a given color in a backward-lower diagonal direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        ox, an integer representing the original row index
;;        oy, an integer representing the original column index
;;        x, an integer representing the current row index
;;        y, an integer representing the current column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        sum, an integer representing the current total of consecutive sequences
;;    Return Value: The total number of consecutive sequences in the specified backward-lower diagonal direction.
;;    Algorithm:
;;        1) Recursively traverse the backward-lower diagonal, checking for consecutive sequences.
;;        2) If a consecutive sequence is found, increment the sum.
;;        3) Continue the search until the entire diagonal has been examined, and then move on to the next diagonal.
;;    Assistance Received: none
;; ********************************************************************* */
(defun sum-consecutive-backwardLower (board color count ox oy x y dx dy sum)
    (cond 
 
        ((< ox 0)
        ;; return the sum
            sum

        )
        (t 
            (cond 
                ((> x 18)
                    ;; start looking at new col
                    ;; move on to the next diagonal
                    (sum-consecutive-backwardLower board color count (- ox 1) oy (- ox 1) oy  dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by 4
                        ((consecutive board color count x y dx dy)
                        
                            (sum-consecutive-backwardLower board color count ox oy (+ x 4) (+ y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by 1 and continue looking
                        (t
                            (sum-consecutive-backwardLower board color count ox oy (+ x 1) (+ y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; /* *********************************************************************
;;    Function Name: sum-consecutive-backwardUpper
;;    Purpose: To calculate the total number of consecutive sequences of pieces of a given color in a backward-upper diagonal direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        ox, an integer representing the original row index
;;        oy, an integer representing the original column index
;;        x, an integer representing the current row index
;;        y, an integer representing the current column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        sum, an integer representing the current total of consecutive sequences
;;    Return Value: The total number of consecutive sequences in the specified backward-upper diagonal direction.
;;    Algorithm:
;;        1) Recursively traverse the backward-upper diagonal, checking for consecutive sequences.
;;        2) If a consecutive sequence is found, increment the sum.
;;        3) Continue the search until the entire diagonal has been examined, and then move on to the next diagonal.
;;    Assistance Received: none
;; ********************************************************************* */
(defun sum-consecutive-backwardUpper (board color count ox oy x y dx dy sum)
    (cond 
        
        ((> oy 16)
        ;; return the sum
            sum

        )
        (t 
            
            (cond 
                ((> y 19)
                    ;; start looking at new col
                    (sum-consecutive-backwardUpper board color count ox (+ oy 1) ox (+ oy 1) dx dy sum)
                )
                (t
                   (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by 4
                    
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-backwardUpper board color count ox oy (+ x 4) (+ y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by 1 and continue looking
                        (t
                            (sum-consecutive-backwardUpper board color count ox oy (+ x 1) (+ y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; 15 19 ox oyoriginX originY of the diagonal search
;; 1 -1 dx dy

;; /* *********************************************************************
;;    Function Name: sum-consecutive-forwardLower
;;    Purpose: To calculate the total number of consecutive sequences of pieces of a given color in a forward-lower diagonal direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        ox, an integer representing the original row index
;;        oy, an integer representing the original column index
;;        x, an integer representing the current row index
;;        y, an integer representing the current column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        sum, an integer representing the current total of consecutive sequences
;;    Return Value: The total number of consecutive sequences in the specified forward-lower diagonal direction.
;;    Algorithm:
;;        1) Recursively traverse the forward-lower diagonal, checking for consecutive sequences.
;;        2) If a consecutive sequence is found, increment the sum.
;;        3) Continue the search until the entire diagonal has been examined, and then move on to the next diagonal.
;;    Assistance Received: none
;; ********************************************************************* */
(defun sum-consecutive-forwardLower (board color count ox oy x y dx dy sum)
    (cond 
 
        ((< ox 0)
        ;; return the sum
            sum
        )
        (t 
            (cond 
                ((> x 18)
                    ;; start looking at new col
                    ;; (print "Row")
                    ;;         (print ox)
                    (sum-consecutive-forwardLower board color count (- ox 1) oy (- ox 1) oy  dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by -4
                        ((consecutive board color count x y dx dy)
                        
                            (sum-consecutive-forwardLower board color count ox oy (+ x 4) (- y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by -1 and continue looking
                        (t
                            (sum-consecutive-forwardLower board color count ox oy (+ x 1) (- y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; 0 18 => ox oy originX originY of the diagonal search
;; 1 -1 dx dy
;; /* *********************************************************************
;;    Function Name: sum-consecutive-forwardUpper
;;    Purpose: To calculate the total number of consecutive sequences of pieces of a given color in a forward-upper diagonal direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;        count, an integer representing the desired length of the consecutive sequence
;;        ox, an integer representing the original row index
;;        oy, an integer representing the original column index
;;        x, an integer representing the current row index
;;        y, an integer representing the current column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        sum, an integer representing the current total of consecutive sequences
;;    Return Value: The total number of consecutive sequences in the specified forward-upper diagonal direction.
;;    Algorithm:
;;        1) Recursively traverse the forward-upper diagonal, checking for consecutive sequences.
;;        2) If a consecutive sequence is found, increment the sum.
;;        3) Continue the search until the entire diagonal has been examined, and then move on to the next diagonal.
;;    Assistance Received: none
;; ********************************************************************* */
(defun sum-consecutive-forwardUpper (board color count ox oy x y dx dy sum)
    (cond 
 
        ((< oy 4)
        ;; return the sum
            sum
        )
        (t 
            (cond 
                ((< y 1)
                    ;; start looking at new col
                    (sum-consecutive-forwardUpper board color count ox (- oy 1) ox (- oy 1) dx dy sum)
                )
                (t
                   (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by 4
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-forwardUpper board color count ox oy (+ x 4) (- y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by 1 and continue looking
                        (t
                            (sum-consecutive-forwardUpper board color count ox oy (+ x 1) (- y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; /* *********************************************************************
;;    Function Name: total-four-consecutive
;;    Purpose: To determine the total number of sequences of four consecutive pieces of a specified color on the Pente board.
;;    Parameters:
;;        pente-board, a list representing the game board
;;        color, a character representing the color to check for consecutiveness
;;    Return Value: The total number of sequences of four consecutive pieces of the specified color on the board.
;;    Algorithm:
;;        1) Calculate the total number of sequences of four consecutive pieces for each direction (forward-upper, forward-lower, backward-upper, backward-lower, row, and column).
;;        2) Sum the results from each direction to determine the overall total.
;;    Assistance Received: none
;; ********************************************************************* */
(defun total-four-consecutive (pente-board color)
    (+ (sum-consecutive-forwardUpper pente-board color 4 0 18 0 18 1 -1 0) (
        + (sum-consecutive-forwardLower pente-board color 4 15 19 15 19 1 -1 0) (
            + (sum-consecutive-backwardUpper pente-board color 4 0 2 0 2 1 1 0) (
                + (sum-consecutive-backwardLower pente-board color 4 15 1 15 1 1 1 0) (
                    + (sum-consecutive-row pente-board color 4 0 1 0 1 0) (sum-consecutive-col pente-board color 4 0 1 1 0 0)
                )
            )
        )
    )) 
)

;; /* *********************************************************************
;;    Function Name: check-pair
;;    Purpose: To check if two consecutive positions in a specified direction on the game board contain pieces of the opponent's color.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the starting row index
;;        y, an integer representing the starting column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        own-color, a character representing the player's color
;;        opponent-color, a character representing the opponent's color
;;    Return Value: True (t) if two consecutive positions in the specified direction contain pieces of the opponent's color; otherwise, Nil (nil).
;;    Algorithm:
;;        1) Check if the next two positions in the given direction contain pieces of the opponent's color.
;;        2) Perform boundary checks to ensure that the positions are within the game board.
;;        3) Return True (t) if the conditions are met; otherwise, return Nil (nil).
;;    Assistance Received: none
;; ********************************************************************* */
(defun check-pair (board x y dx dy own-color opponent-color)
    ;; in the given direction check if the next two are opponent-color
    ;; also do bound checkinh
    (cond
        ((not (and (<= 0 (+ x dx) 18) (<= 1 (+ y dy) 19) ))
            ()
        )
         ((not (and (<= 0 (+ x (* dx 2)) 18) (<= 1 (+ y (* dy 2)) 19) ))
            ()
        )
        ( (and (equal (get-color board (+ x dx) (+ y dy)) opponent-color) (equal (get-color board (+ x (* dx 2)) (+ y (* dy 2))) opponent-color))
            t
        )
        (t 
            ()
        )
    )
)



;; The reason i did not return reason from this function like for consecutives is that this function is being used
;; in actually capturing pairs and not just determining the possibility

;; /* *********************************************************************
;;    Function Name: capture-pair
;;    Purpose: To check for pairs of opponent's pieces followed by one's own piece in a specified direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the starting row index
;;        y, an integer representing the starting column index
;;        dx, an integer representing the change in row index per step
;;        dy, an integer representing the change in column index per step
;;        own-color, a character representing the player's color
;;        opponent-color, a character representing the opponent's color
;;    Return Value: A list containing the direction as ((dx dy)) if a pair of opponent's pieces followed by one's own piece is found in the specified direction; otherwise, Nil (nil).
;;    Algorithm:
;;        1) Check if the next two positions in the given direction contain opponent-color pieces and the third position contains an own-color piece.
;;        2) Perform boundary checks to ensure that the positions are within the game board.
;;        3) Return the direction as ((dx dy)) in a list if the conditions are met; otherwise, return Nil (nil).
;;    Assistance Received: none
;; ********************************************************************* */
(defun capture-pair (board x y dx dy own-color opponent-color)
    ;; in the given direction check if the next two are opponent-color and the third is own
    ;; also do bound checkinh
            

    (cond
        
        ((not (and (<= 0 (+ x (* dx 3)) 18) (<= 1 (+ y (* dy 3)) 19) ))
            ()
        )
        ( (and (check-pair board x y dx dy own-color opponent-color) (equal (get-color board (+ x (* dx 3)) (+ y (* dy 3))) own-color) )
            (list (list dx dy))
        )
        (t 
            ()
        )
    )
)


;; /* *********************************************************************
;;    Function Name: check-and-capture-pairs
;;    Purpose: To check and capture pairs of opponent's pieces followed by one's own piece in all eight possible directions around a specified position on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        own-color, a character representing the player's color
;;        opponent-color, a character representing the opponent's color
;;    Return Value: An array of directions indicating pairs to capture. Each direction is represented as ((dx dy)) in a list. If no pairs are found, an empty list is returned.
;;    Algorithm:
;;        1) For each of the eight possible directions, check for pairs to capture using the capture-pair function.
;;        2) Combine the results from all eight directions into an array.
;;    Assistance Received: none
;; ********************************************************************* */
(defun check-and-capture-pairs (board x y  own-color opponent-color)
    (append (capture-pair board x  y 0 1 own-color opponent-color) (
        append (capture-pair board x  y 0 -1 own-color opponent-color) (
            append (capture-pair board x  y 1 0 own-color opponent-color) (
                append (capture-pair board x  y -1 0 own-color opponent-color) (
                    append (capture-pair board x  y 1 1 own-color opponent-color) (
                        append (capture-pair board x  y -1 -1 own-color opponent-color) (
                            append (capture-pair board x  y 1 -1 own-color opponent-color) (capture-pair board x  y -1 1 own-color opponent-color)
                        )
                    )
                )
            )
        )
    ) )
)

;; /* *********************************************************************
;;    Function Name: determine-capture-count
;;    Purpose: To determine the capture count for a specified position on the game board by checking and capturing pairs of opponent's pieces followed by one's own piece in all eight possible directions.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        own-color, a character representing the player's color
;;        opponent-color, a character representing the opponent's color
;;    Return Value: An integer representing the number of captures possible from the specified position.
;;    Algorithm:
;;        1) Call the 'check-and-capture-pairs' function to check and capture pairs in all eight possible directions around the specified position.
;;        2) Determine the number of captured pairs by calculating the length of the list of directions returned by 'check-and-capture-pairs'.
;;    Assistance Received: none
;; ********************************************************************* */
(defun determine-capture-count (board x y own-color opponent-color)
    (length (check-and-capture-pairs board x y own-color opponent-color))

)

;; /* *********************************************************************
;;    Function Name: determine-consecutive-count
;;    Purpose: To determine the consecutive count of pieces of a specified color in a given direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the starting position
;;        y, an integer representing the column index of the starting position
;;        dx, an integer representing the change in row index per step in the specified direction
;;        dy, an integer representing the change in column index per step in the specified direction
;;        color, a character representing the color to count consecutiveness
;;        ctr, an integer representing the consecutive count, initially set to 1
;;    Return Value: A list containing an integer representing the consecutive count of pieces of the specified color in the given direction.
;;    Algorithm:
;;        1) Check if the next position in the specified direction is within the game board boundaries.
;;        2) If the color at the next position matches the specified color, increment the consecutive count and continue checking the next position in the same direction.
;;        3) Return the consecutive count as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun determine-consecutive-count (board x y dx dy color ctr)

    (cond 
        ((not (and (<= 0 (+ x dx) 18) (<= 1 (+ y dy) 19)))
            (list ctr)
        )
        ((equal (get-color board (+ x dx) (+ y dy)) color) 
            (determine-consecutive-count board (+ x dx) (+ y dy) dx dy color (+ ctr 1))
        )
        (t 
            (list ctr)
        )
    )
)

;; /* *********************************************************************
;;    Function Name: determine-empty-count
;;    Purpose: To determine the count of empty spaces, in a given direction, that are required to have a feasible sequence of 4 pieces of a specified color on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the starting position
;;        y, an integer representing the column index of the starting position
;;        dx, an integer representing the change in row index per step in the specified direction
;;        dy, an integer representing the change in column index per step in the specified direction
;;        color, a character representing the color to count
;;        ctr, an integer representing the count of empty spaces
;;        emptyRequiredCnt, an integer representing the minimum empty count required to be feasible for a sequence of 4 pieces, initially set to 3
;;    Return Value: A list containing an integer representing the count of empty spaces in the specified direction.
;;    Algorithm:
;;        1) Check if the required empty count has reached 0, and if so, return the count of empty spaces.
;;        2) Check if the next position in the specified direction is within the game board boundaries.
;;        3) If the color at the next position matches the specified color, increment the consecutive count and decrement the required empty count.
;;        4) Return the count of empty spaces as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun determine-empty-count (board x y dx dy color ctr emptyRequiredCnt)

    (cond 
        ((equal emptyRequiredCnt 0)
            (list ctr)
        )
        ((not (and (<= 0 (+ x dx) 18) (<= 1 (+ y dy) 19)))
            (list ctr)
        )
        ((equal (get-color board (+ x dx) (+ y dy)) color) 
            (determine-empty-count board (+ x dx) (+ y dy) dx dy color (+ ctr 1) (- emptyRequiredCnt 1))
        )
        (t 
            (list ctr)
        )
    )
)

;; /* *********************************************************************
;;    Function Name: determine-open-ends
;;    Purpose: To determine the number of open ends in a specified direction for a sequence of consecutive pieces on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the starting position
;;        y, an integer representing the column index of the starting position
;;        dx, an integer representing the change in row index per step in the specified direction
;;        dy, an integer representing the change in column index per step in the specified direction
;;        color, a character representing the color to count
;;        ctr, an integer representing the count of consecutive pieces in the specified direction
;;        consCount, an integer representing the count of consecutive pieces
;;        emptyRequiredCnt, an integer representing the minimum empty count required to be feasible for a sequence of consecutive pieces, initially set to 3
;;    Return Value: A list containing an integer representing the number of open ends in the specified direction.
;;    Algorithm:
;;        1) Determine the count of open positions in the specified direction immediately following the consecutive ones.
;;        2) Return the count of open ends as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun determine-open-ends (board x y dx dy color ctr consCount emptyRequiredCnt)

    (let* 
        (
            ;; openCount is the number of open positions right after the consecutive ones
            (openCount (first  (determine-empty-count board x y (* dx (+ consCount 1)) (* dy (+ consCount 1)) (first '(O)) 0 emptyRequiredCnt)))
        )

        (list openCount)
    )
)

;; /* *********************************************************************
;;    Function Name: five-possible
;;    Purpose: To determine if it's possible to achieve five consecutive pieces of a given color in a specified direction on the game board.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the starting position
;;        y, an integer representing the column index of the starting position
;;        dx, an integer representing the change in row index per step in the specified direction
;;        dy, an integer representing the change in column index per step in the specified direction
;;        color, a character representing the color to count
;;        direction-name, a string representing the name of the direction for use in the output message
;;    Return Value: A list containing two values - 1 if five consecutive pieces are possible, and a message indicating the direction's name; otherwise, 0 and an empty string.
;;    Algorithm:
;;        1) Determine the count of consecutive pieces in both the left and right directions from the specified position.
;;        2) If the total count is greater than or equal to 4, set the result to 1 and create a message indicating "Five Consecutive in <direction-name>".
;;        3) Return the result as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun five-possible (board x y dx dy color direction-name)
    (let* (
        (consecutive-ctr-left (first (determine-consecutive-count board x y dx dy color 0)))
        (consecutive-ctr-right (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
        (left (cond 
            ((>= consecutive-ctr-left 4)
                1
            )
            (t 
                0
            )
        ))
        (right (cond 
            ((>= consecutive-ctr-right 4)
                1
            )
            (t 
                0
            )
        ))
            
        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
        )
    (cond 
            ((>= consecutive-ctr  4)   
                (list 1 (format nil "Five Consecutive in ~A. " direction-name))
            )
            (t 
                (list 0 "")
            )

    )
    )
)


;; /* *********************************************************************
;;    Function Name: five-consecutive
;;    Purpose: To determine the total number of five consecutive pieces in all directions for a specified position on the game board and provide reasons for each direction.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        color, a character representing the color to count
;;    Return Value: A list containing two values - the total number of five consecutive pieces for the position and a concatenated string with reasons for each direction.
;;    Algorithm:
;;        1) Calculate the count of five consecutive pieces in all directions (horizontal, vertical, backward diagonal, forward diagonal) from the specified position.
;;        2) Sum the counts for all directions to obtain the total number of five consecutive pieces.
;;        3) Create a concatenated string with reasons for each direction that has five consecutive pieces.
;;        4) Return the result as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun five-consecutive (board x y color)
   (let 
    (
        (fiveConsCount (+ (first (five-possible board x y 0 1 color "horizontal")) (
        + (first (five-possible board x y 1 0 color "vertical")) (
            + (first (five-possible board x y 1 1 color "backward diagonal")) (first (five-possible board x y 1 -1 color "forward diagonal"))
            )
   ) ))

    (reason (format nil "~A~A~A~A" (first (rest (five-possible board x y 0 1 color "horizontal") )) 
            (first (rest (five-possible board x y 1 0 color "vertical")))
            (first (rest (five-possible board x y 1 1 color "backward diagonal")))
            (first (rest (five-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )
    (list fiveConsCount reason)
   )
)



;; /* *********************************************************************
;;    Function Name: four-possible
;;    Purpose: To determine the priority for a specific position in a given direction for the possibility of forming a sequence of four pieces.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        dx, an integer representing the change in row index per step (direction)
;;        dy, an integer representing the change in column index per step (direction)
;;        color, a character representing the color to check
;;        direction-name, a string representing the name of the direction
;;    Return Value: A list containing two values - the priority for the position and a reason describing the situation.
;;    Algorithm:
;;        1) Calculate the consecutive count on the left and right sides of the specified position in the given direction.
;;        2) Determine the number of open positions immediately to the left and right of the consecutive pieces.
;;        3) Based on the consecutive count and the open positions, assign a priority and provide a descriptive reason.
;;    Assistance Received: none
;; ********************************************************************* */
(defun four-possible (board x y dx dy color direction-name)
    (let* (
            (consecutive-ctr-right (first (determine-consecutive-count board x y dx dy color 0)))
            (right-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-right 1)))
            (consecutive-ctr-left (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
            (left-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-left 1)))

        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
            
        )
    (cond 
            ((= consecutive-ctr  3)
                (cond 
                    ;; tessera formation
                    ((and (> right-open-count 0) (> left-open-count 0))
                        (list 35 (format nil "Both side open four consecutive - ~A. " direction-name))
                    )
                    ((or (> left-open-count 0) (> right-open-count 0) )
                        (list 15 (format nil "One side open Four Consecutive ~A. " direction-name ))
                    )
                    (t 
                       (list 12 (format nil "Four Consecutive ~A. " direction-name))
                    )

                   )    
            )
            (t 
                (list 0 "")
            )

    )
    )
)

;; /* *********************************************************************
;;    Function Name: four-consecutive
;;    Purpose: To determine the total priority for a specified position for the possibility of forming a sequence of four pieces in all directions.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        color, a character representing the color to check
;;    Return Value: A list containing two values - the total priority for the position and a concatenated string with reasons for each direction.
;;    Algorithm:
;;        1) Calculate the priority for forming four consecutive pieces in all directions (horizontal, vertical, backward diagonal, forward diagonal) from the specified position.
;;        2) Sum the priorities for all directions to obtain the total priority.
;;        3) Create a concatenated string with reasons for each direction that has a priority greater than 0.
;;        4) Return the result as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun four-consecutive (board x y color)
   (let 
    (
        (priority (+ (first (four-possible board x y 0 1 color "horizontal")) (
                    + (first (four-possible board x y 1 0 color "vertical")) (
                        + (first (four-possible board x y 1 1 color "backward diagonal")) (first (four-possible board x y 1 -1 color "forward diagonal") )
                        )
            ) ))

        (reason (format nil "~A~A~A~A" (first (rest (four-possible board x y 0 1 color "horizontal") )) 
            (first (rest (four-possible board x y 1 0 color "vertical")))
            (first (rest (four-possible board x y 1 1 color "backward diagonal")))
            (first (rest (four-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )

    (list priority reason)
   
   )
)

;; /* *********************************************************************
;;    Function Name: three-possible
;;    Purpose: To determine the priority for a specific position in a given direction for the possibility of forming a sequence of three pieces.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        dx, an integer representing the change in row index per step (direction)
;;        dy, an integer representing the change in column index per step (direction)
;;        color, a character representing the color to check
;;        direction-name, a string representing the name of the direction
;;    Return Value: A list containing two values - the priority for the position and a reason describing the situation.
;;    Algorithm:
;;        1) Calculate the consecutive count on the left and right sides of the specified position in the given direction.
;;        2) Determine the number of open positions immediately to the left and right of the consecutive pieces.
;;        3) Based on the consecutive count and the open positions, assign a priority and provide a descriptive reason.
;;    Assistance Received: none
;; ********************************************************************* */
(defun three-possible (board x y dx dy color direction-name)
    (let* (
            (consecutive-ctr-right (first (determine-consecutive-count board x y dx dy color 0)))
            (right-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-right 1)))
            (consecutive-ctr-left (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
            (left-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-left 1)))

        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
            
        )

    ;; (print consecutive-ctr)
    (cond 
            ((= consecutive-ctr  2)
                (cond 
                    ;; tessera formation
                    ((and (> right-open-count 0) (> left-open-count 0))
                        (list 9 (format nil "Both side open three consecutive - ~A. " direction-name))
                    )
                    ((or (> left-open-count 0) (> right-open-count 0) )
                        (list 6 (format nil "One side open three Consecutive ~A. " direction-name ))
                    )
                    (t 
                       (list 0 (format nil "" ))
                    )

                   )    
            )
            (t 
                
                (list 0 "")
            )

    )
    )
)

;; /* *********************************************************************
;;    Function Name: three-consecutive
;;    Purpose: To determine the total priority for a specified position for the possibility of forming a sequence of three pieces in all directions.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        color, a character representing the color to check
;;    Return Value: A list containing two values - the total priority for the position and a concatenated string with reasons for each direction.
;;    Algorithm:
;;        1) Calculate the priority for forming three consecutive pieces in all directions (horizontal, vertical, backward diagonal, forward diagonal) from the specified position.
;;        2) Sum the priorities for all directions to obtain the total priority.
;;        3) Create a concatenated string with reasons for each direction that has a priority greater than 0.
;;        4) Return the result as a list.
;;    Assistance Received: none
;; ********************************************************************* */
(defun three-consecutive (board x y color)
   (let 
    (
        (priority (+ (first (three-possible board x y 0 1 color "horizontal")) (
                    + (first (three-possible board x y 1 0 color "vertical")) (
                        + (first (three-possible board x y 1 1 color "backward diagonal")) (first (three-possible board x y 1 -1 color "forward diagonal") )
                        )
            ) ))

        (reason (format nil "~A~A~A~A" (first (rest (three-possible board x y 0 1 color "horizontal") )) 
            (first (rest (three-possible board x y 1 0 color "vertical")))
            (first (rest (three-possible board x y 1 1 color "backward diagonal")))
            (first (rest (three-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )

    (list priority reason)
   
   )
)

;; /* *********************************************************************
;;    Function Name: two-possible
;;    Purpose: To determine the priority for a specific position in a given direction for the possibility of forming a sequence of two pieces.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        dx, an integer representing the change in row index per step (direction)
;;        dy, an integer representing the change in column index per step (direction)
;;        color, a character representing the color to check
;;        directionname, a string representing the name of the direction
;;    Return Value: A list containing two values - the priority for the position and a reason describing the situation.
;;    Algorithm:
;;        1) Determine the color of the opponent.
;;        2) Calculate the consecutive count on both the left and right sides of the specified position in the given direction.
;;        3) Determine the number of open positions immediately to the left and right of the consecutive pieces.
;;        4) Based on the consecutive count, open positions, and neighboring pieces, assign a priority and provide a descriptive reason.
;;    Assistance Received: none
;; ********************************************************************* */
(defun two-possible (board x y dx dy color directionname)
    (let* (
        (enemyColor (cond 
            ((equal (first '(W)) color)
                (first '(B))
            )
            (t
                (first '(W))
            )

        ))
        (consecutive-ctr-left (first (determine-consecutive-count board x y dx dy color 0)))
        (left-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-left 2)))
        (rigtNbrEnemy (cond 
                ;; (+ x (* dx -1)) is giving me error
                ((equal enemyColor (get-color board (+ x (* dx -1)) (+ y (* dy -1))))
                    t
                )
                (t 
                    ()
                )
            ))
        (consecutive-ctr-right (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
        (right-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-right 2)))
        (leftNbrEnemy (cond 
                ((equal enemyColor (get-color board (+ x dx) (+ y dy)))
                    t
                )
                (t 
                    ()
                )
            ))
        (left (cond 
            ((>= consecutive-ctr-left 1)
                1
            )
            (t 
                0
            )
        ))
        (right (cond 
            ((>= consecutive-ctr-right 1)
                1
            )
            (t 
                0
            )
        ))
            
        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 


        )

        
            ;; (print leftNbrEnemy)
            ;; (print rigtNbrEnemy)

   
    (cond 
            ;;TODO FIX THIS if chances of capture, dont return the list
            ;; the following code considers the case Enemy EMpty Own
            ;; Dont place in Empty
            ((and (equal consecutive-ctr 1) (or leftNbrEnemy rigtNbrEnemy) ) 
                
                ;; ()
                (list 0 "")
            )

            ;; for the case
            ;; ENemy Own EMpty, dont place in empty
            ((and (equal consecutive-ctr-left 1) (and (equal consecutive-ctr-right 0) (equal left-open-count 0) ))
                ;; ()
                (list 0 "")
            )
             ((and (equal consecutive-ctr-right 1) (and (equal consecutive-ctr-left 0) (equal right-open-count 0) ))
                ;; ()
                (list 0 "")
            )


            ((and (>= consecutive-ctr  1) (or (> left-open-count 1) (> right-open-count 1) )) 

            
                ;; (list (list left right dx dy))
                (list 2 (format nil "Two consecutives in ~A direction " directionname))
            )
            (t 
                ;; ()
                (list 0 "")
            )

    )
    )
)

;; /* *********************************************************************
;;    Function Name: two-consecutive
;;    Purpose: To determine the total priority for a specified position for the possibility of forming a sequence of two pieces in all directions.
;;    Parameters:
;;        board, a list representing the game board
;;        x, an integer representing the row index of the specified position
;;        y, an integer representing the column index of the specified position
;;        color, a character representing the color to check
;;    Return Value: A list containing two values - the total priority for the position and a concatenated string with reasons for each direction.
;;    Algorithm:
;;        1) Calculate the priority for forming two consecutive pieces in all directions (horizontal, vertical, backward diagonal, forward diagonal) from the specified position.
;;        2) Sum the priorities for all directions to obtain the total priority.
;;        3) Create a concatenated string with reasons for each direction that has a priority greater than 0.
;;        4) Return the result as a list.
;;    Assistance Received: none
;; ********************************************************************* */

(defun two-consecutive (board x y color)
   (let 
    (
        (priority (+ (first (two-possible board x y 0 1 color "horizontal")) (
                    + (first (two-possible board x y 1 0 color "vertical")) (
                        + (first (two-possible board x y 1 1 color "backward diagonal")) (first (two-possible board x y 1 -1 color "forward diagonal") )
                        )
            ) ))

        (reason (format nil "~A~A~A~A" (first (rest (two-possible board x y 0 1 color "horizontal") )) 
            (first (rest (two-possible board x y 1 0 color "vertical")))
            (first (rest (two-possible board x y 1 1 color "backward diagonal")))
            (first (rest (two-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )

    (list priority reason)
   
   )
)

