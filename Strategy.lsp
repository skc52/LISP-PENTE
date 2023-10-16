;; we are creating a board of priorities

(load "Board.lsp")
(load "utility.lsp")



;; function to get the max priority position
;; /* *********************************************************************
;;    Function Name: create-row-priority
;;    Purpose: To create a row of priorities based on the specified row number.
;;    Parameters:
;;        n, an integer representing the row number.
;;    Return Value: Returns a list of priorities for the specified row.
;;    Algorithm:
;;        1) If n is 1, create a row of priorities including labels '0' to 'S'.
;;        2) For any other value of n, create a row with '0' as the first element followed by -1 values.
;;    Assistance Received: none
;; ********************************************************************* */
(defun create-row-priority (n)
  (cond 
    ((= n 1)
     '(0 A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1))))
)

;; /* *********************************************************************
;;    Function Name: create-board-priority
;;    Purpose: To create a board of priorities based on the specified number of rows.
;;    Parameters:
;;        n, an integer representing the number of rows in the board.
;;    Return Value: Returns a 2D list representing the board of priorities.
;;    Algorithm:
;;        1) If n is 0, return nil to indicate an empty board.
;;        2) For any other value of n, create a board by consing rows created by the `create-row-priority` function.
;;    Assistance Received: none
;; ********************************************************************* */
(defun create-board-priority (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row-priority n) (create-board-priority (- n 1)))))
)

;; /* *********************************************************************
;;    Function Name: create-row-reasons
;;    Purpose: To create a row of reasons based on the specified row number.
;;    Parameters:
;;        n, an integer representing the row number.
;;    Return Value: Returns a list of reasons for the specified row.
;;    Algorithm:
;;        1) If n is 1, create a row of reasons including labels 'X' to 'S'.
;;        2) For any other value of n, create a row with '0' as the first element followed by empty strings.
;;    Assistance Received: none
;; ********************************************************************* */

(defun create-row-reasons (n)
  (cond 
    ((= n 1)
     '(X A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""))))
)

;; /* *********************************************************************
;;    Function Name: create-board-reasons
;;    Purpose: To create a board of reasons based on the specified number of rows.
;;    Parameters:
;;        n, an integer representing the number of rows in the board.
;;    Return Value: Returns a 2D list representing the board of reasons.
;;    Algorithm:
;;        1) If n is 0, return nil to indicate an empty board.
;;        2) For any other value of n, create a board by consing rows created by the `create-row-reasons` function.
;;    Assistance Received: none
;; ********************************************************************* */

(defun create-board-reasons (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row-reasons n) (create-board-reasons (- n 1)))))
) 


;; CASE where every empty cell have same priority


;; Check Five consecutive possibility
    ;; if winning with vast difference, prioritize getting points by 4 consecutive or pairs capturing
    ;; else if losing and by 5 consecutive can win the game, maximum priority
    ;; else if losing by vast and ending the game cannot win,then low priority


;; go through each valid position in the board and determine its 5 consecutive
;; start from 0 1 and go upto 18 19 
;; pboard is priority board and board is gameboard


;; /* *********************************************************************
;;    Function Name: set-priority-based-on-5-cons
;;    Purpose: To set priorities for a game board based on the possibility of 5 consecutive pieces.
;;    Parameters:
;;        rboard, the current reason board.
;;        pboard, the current priority board.
;;        board, the game board.
;;        x, the current x-coordinate.
;;        y, the current y-coordinate.
;;        ownColor, the color of the player.
;;        pairsCaptured, the number of pairs captured.
;;        enemyPairsCaptured, the number of enemy pairs captured.
;;        totalScore, the total score of the player.
;;        enemyTotalScore, the total score of the enemy.
;;        fourScore, the score for four consecutive pieces.
;;        enemyFourScore, the score for four consecutive enemy pieces.
;;    Return Value: Returns a pair of updated reason board and priority board.
;;    Algorithm:
;;        1) Check if the current position (x, y) is at the last row and column. If so, return the updated boards.
;;        2) If the current position (x, y) is at the last column, increment the row and continue the search.
;;        3) If the cell at (x, y) is not empty, ignore it and continue to the next cell.
;;        4) Calculate the priority and reason for a 5-consecutive possibility using the `five-consecutive` function.
;;        5) Determine the new priority based on the calculated priority and the game state.
;;        6) Compare the new priority with the current priority in the priority board and update it if the new one is greater.
;;        7) Recursively call the function on the next cell.
;;    Assistance Received: none
;; ********************************************************************* */

(defun set-priority-based-on-5-cons(rboard pboard board x y ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
    
    ;; (print-board rboard)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
        ;;    (print (append (list pboard) (list rboard)))
            (append (list pboard) (list rboard))
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-5-cons rboard pboard board (+ x 1) 1 ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-5-cons rboard pboard board x (+ y 1) ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                    
                    (priority-reason (five-consecutive board x y ownColor))   
                    (reason (first (rest priority-reason)))
                    (fiveCount (first priority-reason))
                    (newPriority 
                            (cond 
                                ((> fiveCount 0)

                                    ;; one of the fivecounts will be counted for 5 consecutive and from the remaining
                                    ;; fivecounts we can collect 1 points as 4 consecutives
                                    (cond 
                                        ((> (- (+ pairsCaptured totalScore fourScore) (+ enemyPairsCaptured enemyTotalScore enemyFourScore)) 4)
                                            (+ 10 (* (- fiveCount 1) 12))
                                        )
                                        ((> (- (+ pairsCaptured totalScore fourScore) (+ enemyPairsCaptured enemyTotalScore enemyFourScore)) 0)
                                            (+ 20 (* (- fiveCount 1) 12))
                                        )
                                        ;; with the 5 game points, you will be able to win, otherwise losing
                                        ((> (- (+ 5 pairsCaptured totalScore fourScore) (+ enemyPairsCaptured enemyTotalScore enemyFourScore)) 0)
                                            (+ 30 (* (- fiveCount 1) 12))
                                        )
                                        ;; even with the 5 points you are losing, take it anyway, 
                                        ;; no guarantee that you will win in the future,
                                        ;; at least this way you are closing the gap
                                        (t 
                                            (+ 40 (* (- fiveCount 1) 12))
                                        )
                                    )
                                )
                                (t 
                                    0

                                )
                            )
                        
                        
                    )
                )

                (cond 
                    ((> newPriority (get-color pboard x y))
                        ;; determine priority factor based on open ends, 
                        ;; based on score difference
                        (set-priority-based-on-5-cons (update-board rboard x y reason) (update-board pboard x y newPriority) board x (+ y 1) ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
                    )
                    (t
                        (set-priority-based-on-5-cons rboard  pboard board x (+ y 1) ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
                    )
                )           
            )
        )
    )
)



;; /* *********************************************************************
;;    Function Name: set-priority-based-on-4-cons
;;    Purpose: To set priorities for a game board based on the possibility of 4 consecutive pieces.
;;    Parameters:
;;        rboard, the current reason board.
;;        pboard, the current priority board.
;;        board, the game board.
;;        x, the current x-coordinate.
;;        y, the current y-coordinate.
;;        ownColor, the color of the player.
;;    Return Value: Returns a pair of updated reason board and priority board.
;;    Algorithm:
;;        1) Check if the current position (x, y) is at the last row and column. If so, return the updated boards.
;;        2) If the current position (x, y) is at the last column, increment the row and continue the search.
;;        3) If the cell at (x, y) is not empty, ignore it and continue to the next cell.
;;        4) Calculate the priority and reason for a 4-consecutive possibility using the `four-consecutive` function.
;;        5) Determine the new priority based on the calculated priority.
;;        6) Compare the new priority with the current priority in the priority board and update it if the new one is greater.
;;        7) Recursively call the function on the next cell.
;;    Assistance Received: none
;; ********************************************************************* */

(defun set-priority-based-on-4-cons(rboard pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            (list pboard rboard)
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-4-cons rboard pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-4-cons rboard pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                    (priority-reason (four-consecutive board x y ownColor))
                    (newPriority (first priority-reason))
                    (reason (first (rest priority-reason)))
                )
                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-4-cons (update-board rboard x y reason) (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-4-cons rboard pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)

;; /* *********************************************************************
;;    Function Name: set-priority-based-on-3-cons
;;    Purpose: To set priorities for a game board based on the possibility of 3 consecutive pieces.
;;    Parameters:
;;        rboard, the current reason board.
;;        pboard, the current priority board.
;;        board, the game board.
;;        x, the current x-coordinate.
;;        y, the current y-coordinate.
;;        ownColor, the color of the player.
;;    Return Value: Returns a pair of updated reason board and priority board.
;;    Algorithm:
;;        1) Check if the current position (x, y) is at the last row and column. If so, return the updated boards.
;;        2) If the current position (x, y) is at the last column, increment the row and continue the search.
;;        3) If the cell at (x, y) is not empty, ignore it and continue to the next cell.
;;        4) Calculate the priority and reason for a 3-consecutive possibility using the `three-consecutive` function.
;;        5) Determine the new priority based on the calculated priority.
;;        6) Compare the new priority with the current priority in the priority board and update it if the new one is greater.
;;        7) Recursively call the function on the next cell.
;;    Assistance Received: none
;; ********************************************************************* */
(defun set-priority-based-on-3-cons(rboard pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            (list pboard rboard)
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-3-cons rboard pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-3-cons rboard pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                   (priority-reason (three-consecutive board x y ownColor))
                    (newPriority (first priority-reason))
                    (reason (first (rest priority-reason)))
                )
                ;; (print newPriority)
                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-3-cons (update-board rboard x y reason) (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-3-cons rboard pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)

;; /* *********************************************************************
;;    Function Name: set-priority-based-on-2-cons
;;    Purpose: To set priorities for a game board based on the possibility of 2 consecutive pieces.
;;    Parameters:
;;        rboard, the current reason board.
;;        pboard, the current priority board.
;;        board, the game board.
;;        x, the current x-coordinate.
;;        y, the current y-coordinate.
;;        ownColor, the color of the player.
;;    Return Value: Returns a pair of updated reason board and priority board.
;;    Algorithm:
;;        1) Check if the current position (x, y) is at the last row and column. If so, return the updated boards.
;;        2) If the current position (x, y) is at the last column, increment the row and continue the search.
;;        3) If the cell at (x, y) is not empty, ignore it and continue to the next cell.
;;        4) Calculate the priority for a 2-consecutive possibility using the `two-consecutive` function.
;;        5) Determine the new priority based on the calculated priority.
;;        6) Compare the new priority with the current priority in the priority board and update it if the new one is greater.
;;        7) Recursively call the function on the next cell.
;;    Assistance Received: none
;; ********************************************************************* */
(defun set-priority-based-on-2-cons(rboard pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            (list pboard rboard)
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-2-cons rboard pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-2-cons rboard pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                    (priority-reason (two-consecutive board x y ownColor))
                    (newPriority (first priority-reason))
                    (reason (first (rest priority-reason)))
                )

                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-2-cons (update-board rboard x y reason) (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-2-cons rboard pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)



;; /* *********************************************************************
;;    Function Name: determine-reason-capture-pairs
;;    Purpose: Determine the reason for capturing pairs in specific directions.
;;    Parameters:
;;        captureDirList, a list of directions where pairs can be captured.
;;        count, the number of directions to check.
;;        reason, the current reason string.
;;    Return Value: The final reason string after considering all directions.
;;    Algorithm:
;;        1) Check if there are no more directions to consider. If so, return the current reason string.
;;        2) Extract the direction (dx, dy) from the captureDirList and consider it.
;;        3) Update the reason string based on the current direction.
;;        4) Recursively call the function for the remaining directions.
;;    Assistance Received: none
;; ********************************************************************* */
(defun determine-reason-capture-pairs (captureDirList count reason)
    (cond 
        ((equal count 0)
            reason
        )
        (t
            (let* 
               
                (
                     (dx (first (first captureDirList)))
                (dy (first (rest (first captureDirList))))
                
                    (

                        
                        reasonUpdated (cond 
                            ((and (equal dx 0 ) (equal dy 1))
                                (format nil "~A, right horizontal " reason )
                            )
                            ((and (equal dx 0 ) (equal dy -1))
                                (format nil "~A, left horizontal " reason )
                            )
                            ((and (equal dx 1 ) (equal dy 0))
                                (format nil "~A, down vertical " reason )
                            )
                            ((and (equal dx -1 ) (equal dy 0))
                                (format nil "~A, up horizontal " reason )
                            )
                            ((and (equal dx 1 ) (equal dy 1))
                                (format nil "~A, down backward diagonal " reason )
                            )
                            ((and (equal dx -1 ) (equal dy -1))
                                (format nil "~A, up backward diagonal " reason )
                            )
                            ((and (equal dx 1 ) (equal dy -1))
                                (format nil "~A, down forward diagonal " reason )
                            )
                            ((and (equal dx -1 ) (equal dy 1))
                                (format nil "~A, up forward diagonal" reason )
                            )
                        )

                    )
                )

                (determine-reason-capture-pairs (rest captureDirList) ( - count 1) reasonUpdated)
            )


            
        )
    )
)



;; /* *********************************************************************
;;    Function Name: set-priority-based-on-pairs-captured
;;    Purpose: Set priorities for a game board based on the possibility of capturing pairs of pieces.
;;    Parameters:
;;        rboard, the current reason board.
;;        pboard, the current priority board.
;;        board, the game board.
;;        x, the current x-coordinate.
;;        y, the current y-coordinate.
;;        ownColor, the color of the player.
;;        pairsCaptured, the number of pairs captured by the player.
;;        enemyPairsCaptured, the number of pairs captured by the enemy.
;;        totalScore, the total score of the player.
;;        enemyTotalScore, the total score of the enemy.
;;        fourScore, the score for four consecutive pieces.
;;        enemyFourScore, the score for four consecutive enemy pieces.
;;    Return Value: Returns a pair of updated reason board and priority board.
;;    Algorithm:
;;        1) Check if the current position (x, y) is at the last row and column. If so, return the updated boards.
;;        2) If the current position (x, y) is at the last column, increment the row and continue the search.
;;        3) Ignore filled cells in the board.
;;        4) Calculate the possible directions for capturing pairs using `check-and-capture-pairs`.
;;        5) Determine the number of pairs that can be captured and construct a reason based on directions.
;;        6) Modify the newPriority based on the number of pairs to capture.
;;        7) Compare the new priority with the current priority in the priority board and update it if the new one is greater.
;;        8) Recursively call the function on the next cell.
;;    Assistance Received: none
;; ********************************************************************* */

(defun set-priority-based-on-pairs-captured(rboard pboard board x y ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            (list pboard rboard)
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-pairs-captured rboard pboard board (+ x 1) 1 ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-pairs-captured rboard pboard board x (+ y 1) ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
        )
        (t  
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update
            (let* 
                (
                    (opponentColor (cond 
                        ((equal ownColor (first '(W)))
                            (first '(B))
                        )
                        (t 
                            (first '(W))
                        )
                    ))
                    (captureDirList (check-and-capture-pairs board x y ownColor opponentColor ))
                    (pairCount (length captureDirList))

                    ;; determining reason based on captureDirList, since check and capture pairs doesnot return reasons
                    (reason (determine-reason-capture-pairs captureDirList ( length captureDirList) ""))
                    (reason (format nil "Capture pairs in ~A " reason))

                    ;; modify newPriority based on conditions
                    ;; if newPriority + pairsCaptured > 5, check the score gap between two players
                    ;; if the gap is substantial and is winning, medium priority
                    ;; if the gap is not that big, then end the game right there, cause enemy might make a comeback in that round
                    ;; if opponent is winning and even with those pairs captured cannot overcome opponent, then negative priority
                    ;; if opponent is winning and with these captures, can win the opponent highest priority

                    (newPriority 
                        (cond 
                            ((> (+ pairCount pairsCaptured) 5)
                                (cond 
                                    ((> (- (+ pairsCaptured totalScore fourScore) (+ enemyPairsCaptured enemyTotalScore enemyFourScore)) 4)
                                        (* pairCount 5)
                                    )
                                    ((> (- (+ pairsCaptured totalScore fourScore) (+ enemyPairsCaptured enemyTotalScore enemyFourScore)) 0)
                                        (* pairCount 10)
                                    )
                                    ((> (- (+ pairCount pairsCaptured totalScore fourScore) (+ enemyPairsCaptured enemyTotalScore enemyFourScore)) 0)
                                        (* pairCount 20)
                                    )
                                    (t 
                                        (* pairCount -10)
                                    )
                                )
                            )
                            (t 
                                (* pairCount 10)
                            )
                        )
                    )
                )
                (cond 
                    ((> newPriority (get-color pboard x y))   
                        (set-priority-based-on-pairs-captured (update-board rboard x y reason) (update-board pboard x y newPriority) board x (+ y 1) ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
                    )
                    (t
                        (set-priority-based-on-pairs-captured rboard pboard board x (+ y 1) ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore)
                    )
                )
            )
        )
    )
)


;; /* *********************************************************************
;;    Function Name: setOwnPriority
;;    Purpose: Set priorities for a game board for the current player.
;;    Parameters:
;;        board, the game board.
;;        ownColor, the color of the current player.
;;        pairsCaptured, the number of pairs captured by the current player.
;;        enemyPairsCaptured, the number of pairs captured by the enemy.
;;        totalScore, the total score of the current player.
;;        enemyTotalScore, the total score of the enemy.
;;    Return Value: Returns a pair of updated reason board and priority board, along with a reason string.
;;    Algorithm:
;;        1) Calculate the total score for four consecutive pieces for both players.
;;        2) Create reason and priority boards for the player.
;;        3) Set priorities based on various conditions and update the boards.
;;        4) Determine the best position and priority for the player.
;;        5) Set priorities for the opponent.
;;        6) Determine the best position and priority for the opponent.
;;        7) Decide the final reason string based on whether the best position and priority have changed.
;;    Assistance Received: none
;; ********************************************************************* */

(defun setOwnPriority (board ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore)
    (let* 
        (
            (enemyColor (cond 
                ((equal (first '(W)) ownColor)
                    (first '(B))
                )
                (t
                    (first '(W))
                )
            ))
            (fourScore (total-four-consecutive board ownColor))
            (enemyFourScore (total-four-consecutive board enemyColor))
            (pboard (create-board-priority 20))
            (rboard (create-board-reasons 20))

            (prboard (set-priority-based-on-5-cons rboard pboard board 0 1 ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-4-cons rboard pboard board 0 1 ownColor))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-3-cons rboard pboard board 0 1 ownColor))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-pairs-captured rboard pboard board 0 1 ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore fourScore enemyFourScore))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-2-cons rboard pboard board 0 1 ownColor))
            (pboard (first prboard))
            (rboard (first (rest prboard)))

            ;; what is the best priority and best position till now?
            ;; if any of these two change we add stop opponent in front of the reason
            (bestPosTillNow (get-best-position rboard pboard -1 -1 -1 0 1 board 3))
            (bestPriorityTillNow (get-color pboard (first bestPosTillNow) (first (rest bestPosTillNow))))

            ;;now determine priorities and best pos for opponent 
            (prboard (set-priority-based-on-5-cons rboard pboard board 0 1 enemyColor enemyPairsCaptured pairsCaptured enemyTotalScore totalScore enemyFourScore fourScore))    
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-4-cons rboard pboard board 0 1 enemyColor))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-3-cons rboard pboard board 0 1 enemyColor))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-pairs-captured rboard pboard board 0 1 enemyColor enemyPairsCaptured pairsCaptured enemyTotalScore totalScore enemyFourScore fourScore ))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            (prboard (set-priority-based-on-2-cons rboard pboard board 0 1 enemyColor))
            (pboard (first prboard))
            (rboard (first (rest prboard)))
            
            (newBestPos (get-best-position rboard pboard -1 -1 -1 0 1 board 3))
            (newbestPriority (get-color pboard (first bestPosTillNow) (first (rest bestPosTillNow))))

            (finalReason (cond 
                ((and (equal bestPosTillNow newBestPos) (equal bestPriorityTillNow newbestPriority))
                    "Reason:"
                )
                (t 
                    "Reason: Stop opponent"
                )
            ))

            
        )
        ;; (print-board pboard)
        (list pboard rboard finalReason)
    )
)


;; /* *********************************************************************
;;    Function Name: choose-randomly
;;    Purpose: Choose an empty cell on the game board randomly.
;;    Parameters:
;;        pboard, the priority board.
;;        board, the game board.
;;        turnNum, the current turn number.
;;    Return Value: Returns a list with the chosen x and y coordinates and a reason.
;;    Algorithm:
;;        1) If it is the third turn, choose a cell that is 3 steps away from the center.
;;        2) Otherwise, choose a random empty cell on the game board.
;;    Assistance Received: none
;; ********************************************************************* */
(defun choose-randomly (pboard board turnNum)
    (cond 
        ;; in the third turn choose 3 steps away from center
        ((equal turnNum 2)
            (let* 
                (
                    (x (random 7))
                    (y (+ (random 6) 1))
                )   
            
            ;; if not taken
            (cond 
                ((and (equal (get-color board x y) (first '(O)))  (equal (get-color pboard x y) 0))
                    (list x y "Randomly chosen")
                )
                (t
                    (choose-randomly pboard board turnNum)
                )
            )
            )
        )
        (t
            (let* 
                (
                    (x (random 19))
                    (y (+ (random 19) 1))
                )   
            
            ;; if not taken
            (cond 
                ((equal (get-color board x y) (first '(O)))
                    (print x)
                    (list x y)
                )
                (t
                    (choose-randomly pboard board turnNum)
                )
            )
            )
        )
    )
)


;; /* *********************************************************************
;;    Function Name: get-best-position
;;    Purpose: Find the cell with the highest priority on the board.
;;    Parameters:
;;        rboard, the reason board.
;;        pboard, the priority board.
;;        bestPriority, the current best priority found.
;;        bestX, the current best X coordinate found.
;;        bestY, the current best Y coordinate found.
;;        x, the current X coordinate to check.
;;        y, the current Y coordinate to check.
;;        board, the game board.
;;        turn, the current turn number.
;;    Return Value: Returns the X and Y coordinates of the cell with the highest priority.
;;    Algorithm:
;;        1) Check if the current position (x, y) is at the last row and column.
;;        2) If it is the last position, return a random choice on turn 2 or the best position on other turns.
;;        3) If the current position is at the last column, increment the row and continue the search.
;;        4) Check the priority of the current cell, and if it is higher than the best priority, update the best values.
;;        5) Recursively call the function for the next cell.
;;    Assistance Received: none
;; ********************************************************************* */

(defun get-best-position(rboard pboard bestPriority bestX bestY x y board turn)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            (cond 
                ((equal turn 2)
                    ( choose-randomly pboard board turn)
                )
                (t
                    ;; (print (get-color rboard bestX bestY))
                    (terpri)
                    (list bestX bestY)
                )
            )
            
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (get-best-position rboard pboard bestPriority bestX bestY (+ x 1) 1 board turn)
        )      
        (t  
            ;; get the priority of that x and y and compare it with bestPriority
            ;; if the priorty > bestPriority, set bestPriority to priority and bestX to x and bestY to y
            (cond 
                ((< bestPriority (get-color pboard x y))   
                    (get-best-position rboard pboard (get-color pboard x y) x y x (+ y 1) board turn)
                )
                (t
                    (get-best-position rboard pboard bestPriority bestX bestY x (+ y 1) board turn)

                )
            )
            
        )
    )
)

;; /* *********************************************************************
;;    Function Name: indices-to-labels
;;    Purpose: Convert row and column indices to labels.
;;    Parameters:
;;        position, a list containing row and column indices.
;;    Return Value: Returns the cell label in the format "A1", "B2", etc.
;;    Algorithm:
;;        1) Extract the row and column indices from the position.
;;        2) Convert the row index to a label by subtracting it from 19.
;;        3) Convert the column index to a label by adding it to the ASCII value of 'A'.
;;        4) Format the cell label and return it.
;;    Assistance Received: none
;; ********************************************************************* */

(defun indices-to-labels(position)

    (let* 
        (
            (row (first position))
            (col (first (rest position)))
            (row-label ( - 19 row))
            (col-label (code-char (+ (+ (- col 1) (char-code #\A)))))
        )
        
        (format t "~c~A" col-label  row-label )
    )
)

;; /* *********************************************************************
;;    Function Name: give-suggestion
;;    Purpose: Provide a suggestion for the next move to the player.
;;    Parameters:
;;        board, the game board.
;;        ownColor, the color of the current player.
;;        pairsCaptured, the number of pairs captured by the current player.
;;        enemyPairsCaptured, the number of pairs captured by the enemy.
;;        totalScore, the total score of the current player.
;;        enemyTotalScore, the total score of the enemy.
;;        turn, the current turn number.
;;    Algorithm:
;;        1) Calculate priority boards and determine the best position.
;;        2) Convert the best position to row and column labels.
;;        3) Print the suggested position and the priority of that cell.
;;    Assistance Received: none
;; ********************************************************************* */
(defun give-suggestion (board ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore turn)

    (let* 
        (
            (prboard (setOwnPriority board ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore))
            (best-position (get-best-position (first (rest prboard)) (first prboard) -1 -1 -1 0 1 board turn))
        )
        ;; (print-board (first prboard))
        ;; (format t "~A ~A" (first (rest (rest prboard))) (get-color (first (rest prboard)) (first best-position) (first (rest best-position))))
        (format t "Suggested position is " )
        (indices-to-labels best-position)
        (terpri)
        (format t "~A ~A" (first (rest (rest prboard))) (get-color (first (rest prboard)) (first best-position) (first (rest best-position))))
        (terpri)

    )

)




