
(load "Board.lisp")
(load "utility.lisp")

(load "Sereialization.lisp")
(defun ask-to-call ()
  (format t "We are tossing a coin to determine who starts the round. What would you like to call?~%")
  (format t "Enter 0 for Heads, 1 for Tails~%")

  (let ((user-choice (read-line)))
    (cond 
      ((string= user-choice "0")
       (format t "You entered Heads~%")
       0)
      ((string= user-choice "1")
       (format t "You entered Tails~%")
       1)
      (t 
       (format t "Invalid input. Please enter 0 for heads or 1 for tails~%")
       (ask-to-call))))
)


(defun toss-coin ()
  ;; random function takes n as an argument and returns a random number from 0 to n-1
  (let ((user-call (ask-to-call))
        (result (random 2)))
    (cond
      ((= result 0)
       (format t "It was head. "))
      (t 
       (format t "It was tails. ")))
    (cond 
      ((= user-call result)
        ;; set human color to W
        ;; set computer color to B
       (format t "You won the call. You will start the game~%")
         '(W B)
        )
      (t
        ;; set human color to B
        ;; set computer color to W
        (format t "You lost the call. Computer will start the game~%")
        '(B W)    
    )
    )
        
   )
)

;;    (give-round-summary hPrevscores cPrevscores h4ConsCount c4ConsCount hCpairs cCpairs hFiveScore cFiveScore hTotScore cTotScore)

(defun give-round-summary ( hPrevscores cPrevscores h4ConsCount c4ConsCount hCpairs cCpairs hFiveScore cFiveScore hTotScore cTotScore)
    ;; show winner
    (cond 
        ((> hTotScore cTotScore)
            (print "HUMAN WON THE ROUND")
        )
        ((< hTotScore cTotScore)
            (print "COMPUTER WON THE ROUND")
        )
        (t 
            (print "THE ROUND WAS A TIE")
        )
    )
    ;; display round scores 
    (princ "Human => ")
    (princ " Captured Pairs: ")
    (princ hCpairs)
    (princ " ,Four consecutive: ")
    (princ h4ConsCount)
    (princ " ,Five Points ")
    (princ hFiveScore)
    (terpri)

    (princ "Computer => ")
    (princ " Captured Pairs: ")
    (princ cCpairs)
    (princ " ,Four consecutive: ")
    (princ c4ConsCount)
    (princ " ,Five Points ")
    (princ cFiveScore)
    (terpri)
)

(defun print-end-of-game-stats (hTotScore cTotScore)

    (print "----------------------END OF TORUNAMENT----------------------------")
    (terpri)
    (princ "Total human score : ")
    (princ hTotScore)
    (terpri)
    (princ "Total computer score : ")
    (princ cTotScore)
    (terpri)

    (cond 
        ((> hTotScore cTotScore)
            (print "Human Won the Tournament")
        )
        (t
            (print "Computer won the Tournament")
        )

    )
)

;; function to print end of game stats
(defun print-end-of-game-stats (hTotScore cTotScore)

    (print "----------------------END OF TORUNAMENT----------------------------")
    (terpri)
    (princ "Total human score : ")
    (princ hTotScore)
    (terpri)
    (princ "Total computer score : ")
    (princ cTotScore)
    (terpri)
     (cond 
        ((> hTotScore cTotScore)
            (print "HUMAN WON THE TOURNAMENT")
        )
        ((< hTotScore cTotScore)
            (print "COMPUTER WON THE TOURNAMENT")
        )
        (t 
            (print "THE TOURNAMENT WAS A TIE")
        )
    )
)

;; function to ask user to continue the tournament
(defun continue-game ()
    (print "Do you want to continue game? Enter 1 for yes, 0 for no.")
    (terpri)
   

  (let ((user-choice (read-line)))
    (cond 
      ((string= user-choice "0")
       (format t "You entered 0~%")
        ()
       )
      ((string= user-choice "1")
       (format t "You entered 1~%")
        t
       )
      (t 
       (format t "Invalid input. Please enter 0 for no or 1 for yes~%")
       (continue-game))))
)

;; resuming a saved game
(defun resume-round ()
    (let* 
        (
            (game-name (ask-to-load-game))
        )

        ;; if game name is not empty then proceed to load the game
        ;; else proceed to start a new round
        (cond 
            ((not game-name)
                (start-round 0 0)
            )
            (t 
                ;;     (board (first colors) (first (rest colors)) nextPlayerColor turn hCPair cCPair 0 0 nextPlayer hTotScore cTotScore)
                (let* 
                    (
                        (game-state (load-game game-name))
                        (board (first game-state))
                        (hColor (first (rest game-state)))
                        (cColor (first (rest (rest game-state))))
                        (playingColor (first (rest (rest (rest game-state)))))
                        (nextPlayerColor (cond 
                                                ((equal playingColor (first '(W)))
                                                    (first '(B))
                                                )
                                                (t
                                                    (first '(W))
                                                )
                        ))
                        (turn (first (rest (rest (rest (rest game-state))))))
                        (hCpair (first (rest (rest (rest (rest (rest game-state)))))))
                        (cCpair (first (rest (rest (rest (rest (rest (rest game-state))))))))
                        (hFiveScore (first (rest (rest (rest (rest (rest (rest (rest game-state)))))))))
                        (cFiveScore (first (rest (rest (rest (rest (rest (rest (rest (rest game-state))))))))))
                        (currentPlayerName (first (rest (rest (rest (rest (rest (rest (rest (rest (rest game-state)))) )))))))
                        (hPrevscores (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest game-state))))) )))))))
                        (cPrevscores (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest game-state)))))) )))))))
                    )

                     (print-stats-during-game hColor cColor nextPlayerColor hCpair cCpair)

                    (let* 
                        (
                            ;; (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )
                            (gamelist (playgame board hColor cColor playingColor turn hCpair cCpair hFiveScore cFiveScore hPrevscores cPrevscores))
                            (humanColor (first (rest gameList)))
                            (comColor (first (rest (rest gameList))))
                            (h4ConsCount (total-four-consecutive (first gamelist)  (first '(W)))) 
                            (c4ConsCount (total-four-consecutive (first gamelist)  (first '(B)) ))
                            (hCpairs (first (rest (rest ( rest gameList)))))
                            (cCpairs (first (rest (rest ( rest  (rest gameList))))))
                            (hFiveScore  (first (rest (rest ( rest (rest ( rest gameList)))))))
                            (cFiveScore  (first (rest (rest ( rest (rest ( rest (rest gameList))))))))
                            (hTotScore (+ h4ConsCount hCpairs hFiveScore ))
                            (cTotScore (+ c4ConsCount cCpairs cFiveScore))                     
                        )

                        ;; determine winner of the round
                    
                        ;; calculate total scores for both
                        ;; display the scores
                        (give-round-summary hPrevscores cPrevscores h4ConsCount c4ConsCount hCpairs cCpairs hFiveScore cFiveScore hTotScore cTotScore)
                        ;; ask user if he wants to continue playing
                        ;; if yes recall this function
                        ;; else ()

                        (cond 
                            ((continue-game)
                                (print "Continuing game...")
                                
                                (start-round (+ hPrevscores hTotScore) (+ cPrevscores cTotScore))
                            )
                            (t
                                ;; print torunament scores determine winner
                                (print-end-of-game-stats (+ hPrevscores hTotScore) (+ cPrevscores cTotScore))                               
                            )                       
                        )           
                    )   
                )
            )
        )
    )
)

;; starting from a new game
;; will return round list
;; (board (hPrevscores cPrevscores) (human-color computer-color) (hCpairs, cCpairs) (hFourCons cFourCons) (hFiveCons cFiveCons) )
(defun start-round (hPrevscores cPrevscores)

    (print "STARTING THE ROUND------------------------------------------------------------------------------")
    ;; create a new board
    ;; ask human to call the toss if hPrevscores and cPrevscores are equal
    ;; toss coin
    ;; else whoever has highest prev score, set their color to W and vice versa
    (let* (
        (board (create-board 20))
        ;; colors[0] is human
        (colors (cond 
                    ((equal hPrevscores cPrevscores)
                        (toss-coin)
                    )
                    ((> hPrevscores cPrevscores)
                        '(W B)
                    )
                    (t 
                        '(B W)
                    )
                )

        )


    )
    ;; call playGame function
    ;; will return the round list when game terminates
    ;; White starts at j10 always


    ;; determine 4 cons
    (print-stats-during-game (first colors) (first (rest colors))  (first '(W)) 0 0)

    (let* 
        (
            ;; (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )
            ;; (first '(B)) because the second player is always black
            (gamelist (playgame (update-board board  9 10 (first '(W))) (first colors) (first (rest colors)) (first '(B)) 1 0 0 0 0 hPrevscores cPrevscores))          
        )

            (cond 
                ((not gameList)
                    (print "Game saved and quitted!")
                )
                (t 
                    (let* 
                        (
                            (humanColor (first (rest gameList)))
                            (comColor (first (rest (rest gameList))))
                            (h4ConsCount (total-four-consecutive (first gamelist)  (first '(W)))) 
                            (c4ConsCount (total-four-consecutive (first gamelist)  (first '(B)) ))
                            (hCpairs (first (rest (rest ( rest gameList)))))
                            (cCpairs (first (rest (rest ( rest  (rest gameList))))))
                            (hFiveScore  (first (rest (rest ( rest (rest ( rest gameList)))))))
                            (cFiveScore  (first (rest (rest ( rest (rest ( rest (rest gameList))))))))
                            (hTotScore (+ h4ConsCount hCpairs hFiveScore ))
                            (cTotScore (+ c4ConsCount cCpairs cFiveScore))
                        )


                        ;; determine winner of the round
           
                        ;; calculate total scores for both
                        ;; display the scores
                        (give-round-summary hPrevscores cPrevscores h4ConsCount c4ConsCount hCpairs cCpairs hFiveScore cFiveScore hTotScore cTotScore)
                        ;; ask user if he wants to continue playing
                        ;; if yes recall this function
                        ;; else ()

                        (cond 
                            ((continue-game)
                                (print "Continuing game...")
                                (start-round (+ hPrevscores hTotScore) (+ cPrevscores cTotScore))
                            )
                            (t
                                ;; print torunament scores determine winner
                                (print-end-of-game-stats (+ hPrevscores hTotScore) (+ cPrevscores cTotScore))   
                            )
                        )
                    )
                )
            )

            
           
    )
   
    )
)

(defun playgame (board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore hPrevscores cPrevscores)
    ;; ask user input
    ;; validate input
    ;; parse input
    ;; check if input is empty
    ;; if empty put the piece in the position
    ;; pass the turn
    (terpri)
    
    ;; (print-stats-during-game hColor cColor playingColor hCPScore cCPScore)

    (print-board board)
    (terpri)
    (print "---------------------------------------------------------------------------------------")
    (terpri)

    ;; ask to save game
    (let* 
            (
                (currentPlayerName (cond 
                                        ((equal playingColor hColor)
                                            'Human
                                        )
                                        (t
                                            'Computer
                                        )

                ))
            )
        
            (cond 
                
                ((save-game board hCPScore hPrevscores cCPScore cPrevscores currentPlayerName playingColor)
                    ;; quit the game
                    ()
                )
                (t

               

    (cond 
        ((or (> hCPScore 4) (> cCPScore 4) )
        ;; (board (hPrevscores cPrevscores) (human-color computer-color) (hCpairs, cCpairs) (hFourCons cFourCons) (hFiveCons cFiveCons) )
            (print "GAME ENDED")
            (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )

        )

        ((or (equal hFiveScore 5) (equal cFiveScore 5))
            (print cCPScore)
            (print "GAME ENDED")
            (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )
        )
        (t
                            (let* (
                            (nextColor 
                                (cond 
                                    ((equal playingColor (first '(W)))
                                        (first '(B))
                                    )
                                    (t
                                        (first '(W))
                                    )
                                )
                            )
                            (position (parse-position (first (ask-and-validate))));; col and row
                            (input-x (first (rest position)))
                            (input-y (first position))
                            ;; check if position is empty
                            (isEmpty (equal (get-color board (first (rest position))  (first position)) (first '(O))))
                        )
                        ;; print the board at the start of each game iteration
                        
                        (cond

                            ((and (equal turn 2) (not (or (or (<= 4  (- input-x 9) 18) (<= -18 (- input-x 9) -4)) (or (<= 4  (- input-y 10) 18) (<= -18 (- input-y 10) -4)))) ) 
                                (print "Cannot put within 3 steps in 2nd turn of White. Enter new position")
                                (terpri)
                                (playGame board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore hPrevscores cPrevscores)
                            ) 
                            ((not isEmpty)
                                (print "Position is not empty. Put somewhere else")
                                (terpri)
                                (playGame board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore hPrevscores cPrevscores)
                            )
                            ;; is empty then update the board
                            (t 
                                ;; check for captured pairs, if 5 then return 
                                
                                ;; check for 5 cons, if true then return
                                (let* 
                                    (
                                        (capture-dir-list (check-and-capture-pairs board input-x input-y playingColor nextColor))
                                        (captured-board (check-capture-update board capture-dir-list input-x input-y playingColor nextColor))
                                        (fiveCtr (length (five-consecutive captured-board input-x input-y playingColor)))
                                        (hFiveUpdate (cond
                                            ((and (equal playingColor hColor) (> fiveCtr 0) ) 
                                                5
                                            )
                                            (t
                                                0                                            
                                            )

                                        ))
                                        (cFiveUpdate (cond
                                            ((and (equal playingColor cColor) (> fiveCtr 0) ) 
                                                5
                                            )
                                            (t
                                                0                                            
                                            )

                                        ))
                                        (hCapScores (cond 
                                                ((and (equal playingColor hColor))
                                                    (+ hCPScore (length capture-dir-list))
                                                )
                                                (t
                                                    hCPScore
                                                )
                                            
                                            )
                                        )

                                        (cCapScores (cond 
                                                ((and (equal playingColor cColor))
                                                    (+ cCPScore (length capture-dir-list))
                                                )
                                                (t 
                                                    cCPScore
                                                )
                                            )


                                        )

                                    )
                                        (print "---------------------------------------------------------------------------------------")
                                        (terpri)

                                        (print-stats-during-game hColor cColor playingColor hCapScores cCapScores)
                                        
                                    (playgame (update-board captured-board input-x input-y playingColor) hColor cColor nextColor (+ turn 1) hCapScores cCapScores hFiveUpdate cFiveUpdate hPrevscores cPrevscores)
                                )
                            )
                        )
                    )
        )
    )
     )
            )
    )
)

;; captures the pairs for real
(defun capture-pairs-real (board x y capture-dir-list length-list)
    (let*
        (
            (dx 
                (cond 
                    ((not (equal length-list 0))
                        (first (first capture-dir-list))
                    )
                    (t
                        0
                    )
                )
            )
            
            (dy 
                (cond 
                    ((not (equal length-list 0))
                        (first (rest (first capture-dir-list)))
                    )
                    (t
                        0
                    )
                )
            )
        )    
    
        (cond
            ((equal length-list 0)
                board
            )
            (t
                ;; board
                (capture-pairs-real (update-board (update-board board (+ x (* 2 dx )) (+ y (* 2 dy)) (first '(O))) (+ x dx) (+ y dy) (first '(O)) ) x y (rest capture-dir-list) (length (rest capture-dir-list)))
            )
            
        )

    )
)

(defun check-capture-update ( board capture-dir-list input-x input-y playingColor nextColor)
    (capture-pairs-real board input-x input-y capture-dir-list  (length capture-dir-list))
)

(defun ask-and-validate()
    (let* (
        (pos-input (ask-user-position-input))
        (validated (validate-user-input pos-input))
    )

    (cond 
        ((not validated)
            (ask-and-validate)
        )
        (t
            (list pos-input)
        )
    )

    )
)

(defun print-stats-during-game(hColor cColor currentColor hCPScores cCPScores)
    (princ "Human is ")
    (princ hColor)
    (terpri)
    (princ "Computer is ")
    (princ cColor)
    (terpri)
    (princ "Human Captured Pairs: ")
    (princ hCPScores)
    (terpri)
    (princ "Computer Captured Pairs: ")
    (princ cCPScores)
    (terpri)

    (cond 
        ((equal hColor currentColor)
            (princ "Next Player: Computer")
            (terpri)
        )
        (t 
            (princ "Next Player: Human")
            (terpri)
        )
    )

)

;; (start-round 0 0)
;; (resume-round)