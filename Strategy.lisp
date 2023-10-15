;; we are creating a board of priorities

(load "Board.lisp")
(load "utility.lisp")
;; (load "Round.lisp")
;; check four consecutive possibility
    ;; both ends open, high priority
;; check three consecutive possibility
    ;; both ends open, semi high priority
    ;; if no prospect of becoming a 4 or a 5, no priority
    ;; else medium priority
;; check capture possibility
    ;; if one end open, sure capture, high priority
    ;; if with this capture wins the game
        ;; if winning by major difference, maximum priority
        ;; if winning but not much difference, high priority
        ;; if losing and with this capture can draw, take it
        ;; if losing and with this capture will lose for sure, negative priority
    ;; if both ends open semi medium proirty

;; check being captured possibility
    ;; if both ends open, high priority
    ;; if one end open, maximim priority


;; function to get the max priority position

(defun create-row-priority (n)
  (cond 
    ((= n 1)
     '(0 A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)))))

(defun create-board-priority (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row-priority n) (create-board-priority (- n 1))))))


(defun create-row-reasons (n)
  (cond 
    ((= n 1)
     '(X A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "")))))

(defun create-board-reasons (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row-reasons n) (create-board-reasons (- n 1)))))) 


;; CASE where every empty cell have same priority


;; Check Five consecutive possibility
    ;; if winning with vast difference, prioritize getting points by 4 consecutive or pairs capturing
    ;; else if losing and by 5 consecutive can win the game, maximum priority
    ;; else if losing by vast and ending the game cannot win,then low priority


;; go through each valid position in the board and determine its 5 consecutive
;; start from 0 1 and go upto 18 19 
;; pboard is priority board and board is gameboard
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

;; this should only get priority if there is a possibility od 4 cons or more
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

;; this should only get priority if there is a possibility od 4 cons or more
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
;; complementary functions will be checking open ends, if so adding to the priority



;; function to determine reason for capture pairs
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



;; for sure capture
;; check-and-capture-pairs
;; NEED TO UNIT TEST BUT I THINK IS COMPLETE
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
            ;; (pboard (set-priority-based-on-3-cons pboard board 0 1 ownColor))
            ;; (pboard (set-priority-based-on-2-cons pboard board 0 1 ownColor))

            ;; now from enemy's point of view
            ;; (prboard (set-priority-based-on-5-cons rboard pboard board 0 1 enemyColor enemyPairsCaptured pairsCaptured enemyTotalScore totalScore enemyFourScore fourScore))
            ;; (pboard (first prboard))
            ;; (rboard (first (rest prboard)))
            ;; (pboard (set-priority-based-on-4-cons pboard board 0 1 enemyColor))
            ;; (pboard (set-priority-based-on-pairs-captured pboard board 0 1 enemyColor enemyPairsCaptured pairsCaptured enemyTotalScore totalScore enemyFourScore fourScore ))
            ;; (pboard (set-priority-based-on-3-cons pboard board 0 1 enemyColor))
            ;; (pboard (set-priority-based-on-2-cons pboard board 0 1 enemyColor))
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

            
        )
        ;; (print-board rboard)
        (list pboard rboard)
    )
)


;; CASE where every empty cell have same priority
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

;; TODO, dont set yourself up for capture

;; function to get the max priority x and y
;; go through the entire priority board and pick out the max priority row and col
(defun get-best-position(rboard pboard bestPriority bestX bestY x y board turn)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            (cond 
                ((equal turn 2)
                    ( choose-randomly pboard board turn)
                )
                (t
                    (print (get-color rboard bestX bestY))
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

;; function to convert indices to labels
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

;; convert best position to row and col labels and show them as suggestions
(defun give-suggestion (board ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore turn)

    (let* 
        (
            (prboard (setOwnPriority board ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore))
            (best-position (get-best-position (first (rest prboard)) (first prboard) -1 -1 -1 0 1 board turn))

            ;; convert best position to labels
            ;; (row (first best-position))
            ;; (col (first (rest best-position)))
            ;; (row-label ( - 19 row))
            ;; (col-label (code-char (+ (+ (- col 1) (char-code #\A)))))
            
        )

        (print (get-color (first (rest prboard)) (first best-position) (first (rest best-position))))
         (format t "Suggested position is " )
         (indices-to-labels best-position)
         (terpri)

    )

)

;; TODO convert position to labels

;; TESTING setOwnPriority
(let* 
    (
        (board '((O O O O O O O O O O O O O O O O O O O O)
            (O O O O O O O O O O O O O O O W B B O O)
            (O O O O O O O O O O O O O O O O O B B O)
            (O O O O O O O O O O O W O O O O B O B O)
            (O O O O O O O O B W W O W B O W B W W O)
            (O O O O O O O O O O O W O O O O B O O O)
            (O O O O O O O O O O O W O O O O O O O O)
            (O O O O O O O O O O O O O O O O O O O O)
            (O O O O O O O O O O O O O O O O O O O O)
            (O O O B O W W B O O O O O O O O O O O O)
            (O O O O O O O O W W O W W W O O O O O O)
            (O O O O O O O O O O O O O O O O O O O O)
            (O O O O O W O O O O O O O O O O O O O O)
            (O O O O O W O O O O O O O O O O O O O O)
            (O O W W W O O O O O O O O O O O O O O O)
            (O O O O O W O O O O O O O O O O O W O O)
            (O O O O O W O O O O O O O O O O O O O O)
            (O O O O O O O O O O O O O O O O O W O O)
            (O O O O O O O O O O O O O O O O O O O O))
        )
        ;; (defun setOwnPriority (board ownColor pairsCaptured enemyPairsCaptured totalScore enemyTotalScore)

        (pboard (setOwnPriority board (first '(W)) 2 1 1 4))
    )
    ;; (print-board pboard)
    ;; (print (get-best-position pboard -1 -1 -1 0 1 board 3))

    ;; (give-suggestion board (first '(W)) 2 1 1 4 0)
    


)



