
(load "Board.lsp")
(load "utility.lsp")
(load "Sereialization.lsp")
(load "Strategy.lsp")


;; /* *********************************************************************
;;    Function Name: ask-to-call
;;    Purpose: Prompt the player to call Heads or Tails for a coin toss.
;;    Parameters: None.
;;    Return Value: Returns 0 if the player called Heads, 1 if the player called Tails.
;;    Algorithm:
;;        1) Prompt the player for their call, either "0" for Heads or "1" for Tails.
;;        2) Check the user's input and return the corresponding value.
;;        3) If the input is invalid, prompt again until a valid choice is made.
;;    Assistance Received: none
;; ********************************************************************* */

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

;; /* *********************************************************************
;;    Function Name: toss-coin
;;    Purpose: Simulate a coin toss and determine the starting player.
;;    Parameters: None.
;;    Return Value: Returns a list representing the chosen colors for the human player and the computer player.
;;    Algorithm:
;;        1) Ask the player to call Heads or Tails using the `ask-to-call` function.
;;        2) Generate a random result (0 for Heads, 1 for Tails).
;;        3) Compare the user's call with the result to determine the starting player.
;;        4) Return a list with the chosen colors for the human and computer players.
;;    Assistance Received: none
;; ********************************************************************* */

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


;; /* *********************************************************************
;;    Function Name: give-round-summary
;;    Purpose: Display the summary of the current round.
;;    Parameters:
;;        hPrevscores, the previous scores of the human player.
;;        cPrevscores, the previous scores of the computer player.
;;        h4ConsCount, the count of four consecutive for the human player.
;;        c4ConsCount, the count of four consecutive for the computer player.
;;        hCpairs, the number of pairs captured by the human player.
;;        cCpairs, the number of pairs captured by the computer player.
;;        hFiveScore, the five-point score of the human player.
;;        cFiveScore, the five-point score of the computer player.
;;        hTotScore, the total score of the human player.
;;        cTotScore, the total score of the computer player.
;;    Return Value: None.
;;    Algorithm:
;;        1) Display the winner of the round (Human, Computer, or Tie).
;;        2) Display the summary of scores and statistics for both players.
;;    Assistance Received: none
;; ********************************************************************* */

(defun give-round-summary ( hPrevscores cPrevscores h4ConsCount c4ConsCount hCpairs cCpairs hFiveScore cFiveScore hTotScore cTotScore)
    ;; show winner
    (cond 
        ((> hTotScore cTotScore)
            (format t "HUMAN WON THE ROUND")
            (terpri)
        )
        ((< hTotScore cTotScore)
            (format t "COMPUTER WON THE ROUND")
            (terpri)
        )
        (t 
            (format t "THE ROUND WAS A TIE")
            (terpri)
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


;; /* *********************************************************************
;;    Function Name: print-end-of-game-stats
;;    Purpose: Display the end-of-game statistics and determine the tournament winner.
;;    Parameters:
;;        hTotScore, the total score of the human player.
;;        cTotScore, the total score of the computer player.
;;    Return Value: None.
;;    Algorithm:
;;        1) Display the end-of-tournament results, including total scores.
;;        2) Determine and display the tournament winner (Human, Computer, or Tie).
;;    Assistance Received: none
;; ********************************************************************* */
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

    ;; /* *********************************************************************
    ;;    Function Name: continue-game
    ;;    Purpose: Ask the user if they want to continue the tournament.
    ;;    Parameters: None.
    ;;    Return Value: Returns true if the user chooses to continue (enters "1"), false if the user chooses not to continue (enters "0").
    ;;    Algorithm:
    ;;        1) Prompt the user to enter their choice (1 for yes, 0 for no).
    ;;        2) Check the user's input and return true or false accordingly.
    ;;        3) If the input is invalid, prompt again until a valid choice is made.
    ;;    Assistance Received: none
    ;; ********************************************************************* */
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



;; *********************************************************************
;; Function Name: resume-round
;; Purpose: To handle resuming a saved game or starting a new round if no saved game is available.
;; Parameters: None.
;; Return Value: None.
;; Algorithm:
;;   1) Prompt the user to load a saved game or start a new round.
;;   2) If a saved game is chosen (game name provided), load the game state, initialize variables, and continue the game.
;;   3) If no saved game is chosen, start a new round with the specified variables.
;;   4) Display round summary and ask the user if they want to continue the tournament.
;;   5) If the user chooses to continue, start a new round; otherwise, end the tournament.
;; Assistance Received: none
;; *********************************************************************
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

                     (print-stats-during-game hColor cColor nextPlayerColor hCpair cCpair hPrevscores cPrevscores)

                    (let* 
                        (
                            (gamelist (playgame board hColor cColor playingColor turn hCpair cCpair hFiveScore cFiveScore hPrevscores cPrevscores))                
                        )
                        (after-game hPrevscores cPrevscores board (list hColor cColor) gamelist)
                    )   
                )
            )
        )
    )
)

;; *********************************************************************
;; Function Name: start-round
;; Purpose: Start a new round in the tournament or continue the previous round.
;; Parameters: hPrevscores - Human's previous round score, cPrevscores - Computer's previous round score.
;; Return Value: None.
;; Algorithm:
;;   1) Initialize a new game board of size 20x20.
;;   2) Determine the player colors based on the previous round scores:
;;        - If the previous round scores are equal, toss a coin to decide the colors.
;;        - If the human's previous score is higher, set human's color to White (W) and computer's color to Black (B).
;;        - If the computer's previous score is higher, set human's color to Black (B) and computer's color to White (W).
;;   3) Display the starting information and player colors.
;;   4) Call the 'playgame' function to play the round, and get the resulting round list.
;;   5) After the round, calculate round scores, display the round summary, and ask the user if they want to continue the tournament.
;;   6) If the user chooses to continue, start a new round with updated scores.
;;   7) If the user chooses to end the tournament, print the end-of-tournament stats.
;; Assistance Received: none
;; *********************************************************************

(defun start-round (hPrevscores cPrevscores)

    (format t "STARTING THE ROUND------------------------------------------------------------------------------")
    (terpri)
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
    (print-stats-during-game (first colors) (first (rest colors))  (first '(W)) 0 0 hPrevscores cPrevscores)

    ;; call playGame function
    ;; will return the round list when game terminates
    ;; White starts at j10 always
    ;; determine 4 cons
    (let* 
        (
            ;; (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )
            ;; (first '(B)) because the second player is always black
            (gamelist (playgame (update-board board  9 10 (first '(W))) (first colors) (first (rest colors)) (first '(B)) 1 0 0 0 0 hPrevscores cPrevscores))          
        )
        (after-game hPrevscores cPrevscores board colors gamelist)     
    )
    )
)

;; *********************************************************************
;; Function Name: need-help
;; Purpose: Ask the user if they need help in choosing a position for their game piece.
;; Parameters: None.
;; Return Value: Returns true (T) if the user chooses to receive help (enters "1"), false (NIL) if the user chooses not to receive help (enters "0").
;; Algorithm:
;;   1) Display a message asking the user if they need help.
;;   2) Read the user's input.
;;   3) Check the input, and return true or false based on the user's choice.
;;   4) If the input is invalid, prompt the user again until a valid choice is made.
;; Assistance Received: none
;; *********************************************************************
(defun need-help ()
  (format t "Do you need help? (Enter 0 for 'no' or 1 for 'yes')~%")


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
       (need-help))))
)

;; *********************************************************************
;; Function Name: playgame
;; Purpose: Play a round of the game and manage the game's progress.
;; Parameters:
;;   - board: The current game board.
;;   - hColor: Human player's color.
;;   - cColor: Computer player's color.
;;   - playingColor: The color of the current player (human or computer).
;;   - turn: The current turn number.
;;   - hCPScore: Human's current captured pairs score.
;;   - cCPScore: Computer's current captured pairs score.
;;   - hFiveScore: Human's five consecutive score.
;;   - cFiveScore: Computer's five consecutive score.
;;   - hPrevscores: List of previous scores for the human player.
;;   - cPrevscores: List of previous scores for the computer player.
;; Return Value: A list containing the current state of the game after the round.
;; Algorithm:
;;   1) Check if the game has ended by capturing five pairs or five consecutive pieces.
;;   2) If the game hasn't ended, display the game board and current player.
;;   3) Ask the player if they need help in choosing a position.
;;   4) Depending on the player's color and choice for help, provide a suggestion or let the player decide.
;;   5) Determine the next player's color.
;;   6) Capture pairs and calculate the new score.
;;   7) Recursively call 'playgame' with the updated board and game state.
;;   8) Return the updated game state after the round.
;; Assistance Received: none
;; *********************************************************************

(defun playgame (board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore hPrevscores cPrevscores)
    (print cFiveScore)
    (cond 
        ;; check for captured pairs, if 5 then return 
        ((or (> hCPScore 4) (> cCPScore 4) )
            (format t "GAME ENDED")
            (terpri)
            (print-board board)

            (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )

        )
        ;; check for 5 cons, if true then return
        ((or (equal hFiveScore 5) (equal cFiveScore 5))
            (print cCPScore)
            (format t "GAME ENDED")
            (terpri)
            (print-board board)

            (list board hColor cColor hCPScore cCPScore hFiveScore cFiveScore )
        )
        ;; game can continue
        (t 

        ;; ask user input
        ;; validate input
        ;; parse input
        ;; check if input is empty
        ;; if empty put the piece in the position
        ;; pass the turn
        (terpri)
        (print-board board)
        (terpri)
        (print "---------------------------------------------------------------------------------------")
        (terpri)

        ;; ask to save game
        (let* 
            (
                (currentPlayerName 
                    (cond 
                        ((equal playingColor hColor)
                            'Human
                        )
                        (t
                            'Computer
                        )

                    )
                )
            )
            
        (cond 
            ((save-game board hCPScore hPrevscores cCPScore cPrevscores currentPlayerName playingColor)
                ;; quit the game
                ()
            )
            (t
                (cond 
                    ;; if it is computer turn dont ask for help    
                    ;; else provide option to ask for help
                    ( (and (equal playingColor hColor) (need-help))
                        (cond 
                            ((equal playingColor hColor)
                                (give-suggestion board hColor hCPScore cCPScore hPrevscores cPrevscores turn )
                            )
                            (t
                                (give-suggestion board cColor cCPScore hCPScore cPrevscores hPrevscores  turn )
                            )
                        )
                    )
                )

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
                        (priority-reason (setOwnPriority board playingColor cCPScore hCPScore cPrevscores hPrevscores))

                        (position (cond 
                            ;; if human ask for position
                            ((equal playingColor hColor)
                                
                                (parse-validated-pos turn board)
                            )
                            (t 
                                
                                (get-best-position (first (rest priority-reason)) (first priority-reason) -1 -1 -1 0 1 board turn)
                                

                            )
                        ))
                        (input-x (first position))
                        (input-y (first (rest position)))
                        )

                        ;; print the position chosen
                        (cond 
                            ((equal playingColor cColor)
                               (print-board (first priority-reason))
                                (format t "Computer placed its piece on ")
                                 ;; prints out the positions
                                (indices-to-labels position)
                                (terpri)
                                (format t "~A ~A" (first (rest (rest priority-reason))) (get-color (first (rest priority-reason)) (first position) (first (rest position))) )
                            )
                            (t 
                                (format t "You placed your piece on ")
                                 ;; prints out the positions
                                (indices-to-labels position)
                            )
                        )
                    

    
                        (let* 
                            (
                                (capture-dir-list (check-and-capture-pairs board input-x input-y playingColor nextColor))
                                (captured-board (check-capture-update board capture-dir-list input-x input-y playingColor nextColor))
                                (fiveCtr (first (five-consecutive captured-board input-x input-y playingColor)))
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
                            (print-stats-during-game hColor cColor playingColor hCapScores cCapScores hPrevscores cPrevscores)  
                            (print cFiveUpdate)  
                            (playgame (update-board captured-board input-x input-y playingColor) hColor cColor nextColor (+ turn 1) hCapScores cCapScores hFiveUpdate cFiveUpdate hPrevscores cPrevscores)
                        )                            
                )   
            )
            )
        )
        )
    )
)


;; *********************************************************************
;; Function Name: after-game
;; Purpose: Perform actions after a game has ended, such as displaying scores and allowing the user to continue or quit.
;; Parameters:
;;   - hPrevscores: The previous score of the human player.
;;   - cPrevscores: The previous score of the computer player.
;;   - board: The game board.
;;   - colors: A list representing the colors of the human and computer players.
;;   - gamelist: The list representing the game state.
;; Return Value: None.
;; Algorithm:
;;   1) Check if gamelist is empty, indicating whether the game was saved or not.
;;   2) If gamelist is empty, display a message that the game was saved and quit.
;;   3) If gamelist is not empty, calculate the scores and winner of the game.
;;   4) Display the scores and other relevant game information.
;;   5) Ask the user if they want to continue playing.
;;   6) If the user chooses to continue, start a new round with updated scores.
;;   7) If the user chooses to quit, print end-of-game statistics.
;; Assistance Received: none
;; *********************************************************************

(defun after-game(hPrevscores cPrevscores board colors gamelist)
            (cond 
                ((not gamelist)
                    (format t "Game saved and quitted!")
                    (terpri)
                )
                (t 
                    (let* 
                        (
                            (humanColor (first (rest gameList)))
                            (comColor (first (rest (rest gameList))))
                            (hFiveScore  (first (rest (rest ( rest (rest ( rest gameList)))))))
                            (cFiveScore  (first (rest (rest ( rest (rest ( rest (rest gameList))))))))

                            ;; subtract one from the four cons of the winner if the winner won by 5consecutive
                            (h4ConsCount 
                                (cond 
                                    ((equal hFiveScore 5)
                                        (- (total-four-consecutive (first gamelist)  (first colors)) 1)
                                    )
                                    (t 
                                        (total-four-consecutive (first gamelist)  (first colors))
                                    )
                                )
                            ) 
                            (c4ConsCount 
                                (cond 
                                    ((equal cFiveScore 5)
                                        (- (total-four-consecutive (first gamelist)  (first (rest colors)) ) 1)
                                    )
                                    (t 
                                        (total-four-consecutive (first gamelist)  (first (rest colors)) )
                                    )
                                )
                            ) 
                            ;; (c4ConsCount (total-four-consecutive (first gamelist)  (first '(B)) ))
                            (hCpairs (first (rest (rest ( rest gameList)))))
                            (cCpairs (first (rest (rest ( rest  (rest gameList))))))
                            
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
                                (format t "Continuing game...")
                                (terpri)
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

;; *********************************************************************
;; Function Name: capture-pairs-real
;; Purpose: Capture pairs in a real game by updating the game board.
;; Parameters:
;;   - board: The current game board.
;;   - x: The x-coordinate of the current move.
;;   - y: The y-coordinate of the current move.
;;   - capture-dir-list: A list of capture directions.
;;   - length-list: The length of the capture direction list.
;; Return Value: The updated game board after capturing pairs.
;; Algorithm:
;;   1) Check if the length of the capture direction list is 0.
;;   2) If it's 0, return the current board (no captures).
;;   3) If it's not 0, update the board by capturing pairs in the specified directions.
;;   4) Recursively call the function with updated coordinates and direction list.
;;   5) Repeat the process until all pairs have been captured.
;; Assistance Received: none
;; *********************************************************************
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


;; *********************************************************************
;; Function Name: check-capture-update
;; Purpose: Update the game board by capturing pairs in specified directions.
;; Parameters:
;;   - board: The current game board.
;;   - capture-dir-list: A list of capture directions.
;;   - input-x: The x-coordinate of the current move.
;;   - input-y: The y-coordinate of the current move.
;;   - playingColor: The color of the current player.
;;   - nextColor: The color of the next player.
;; Return Value: The updated game board after capturing pairs.
;; Algorithm:
;;   1) Call the `capture-pairs-real` function with the board, coordinates, capture direction list, and its length.
;;   2) `capture-pairs-real` captures pairs by updating the board in the specified directions.
;;   3) Return the updated board.
;; Assistance Received: none
;; *********************************************************************

(defun check-capture-update ( board capture-dir-list input-x input-y playingColor nextColor)
    (capture-pairs-real board input-x input-y capture-dir-list  (length capture-dir-list))
)

;; *********************************************************************
;; Function Name: ask-and-validate
;; Purpose: Ask the user for a position input and validate it.
;; Parameters: None.
;; Return Value: A list containing a validated position input.
;; Algorithm:
;;   1. Ask the user for a position input using a function (e.g., ask-user-position-input).
;;   2. Validate the input using a function (e.g., validate-user-input).
;;   3. If the input is not validated, repeat the process by calling the function recursively.
;;   4. If the input is validated, return it as a list.
;; Assistance Received: none
;; *********************************************************************

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

;; *********************************************************************
;; Function Name: parse-validated-pos
;; Purpose: Parse and validate a user's position input.
;; Parameters:
;;   - turn: The current turn of the game.
;;   - board: The game board.
;; Return Value: A list containing the validated x and y coordinates of the selected position.
;; Algorithm:
;;   1. Ask the user for a position input using a function (e.g., ask-and-validate).
;;   2. Parse the input position to extract x and y coordinates.
;;   3. Check if the input is within specific constraints based on the turn.
;;   4. Verify that the selected position on the board is empty.
;;   5. If the input is valid, return a list containing the x and y coordinates.
;;   6. If the input is invalid, provide appropriate messages and prompt the user to enter a new position.
;; Assistance Received: none
;; *********************************************************************

(defun parse-validated-pos (turn board)
    (let* 
        (
             ;; parse position returns col as first in the list and then row
            (parsedPos (parse-position (first (ask-and-validate))))
            (input-x (first (rest parsedPos)))
            (input-y (first parsedPos))
            (isEmpty (equal (get-color board input-x input-y) (first '(O))))

        )
        (cond 
            ((and (equal turn 2) (not (or (or (<= 4  (- input-x 9) 18) (<= -18 (- input-x 9) -4)) (or (<= 4  (- input-y 10) 18) (<= -18 (- input-y 10) -4)))) ) 
                (print "Cannot put within 3 steps in 2nd turn of White. Enter new position")
                (terpri)
                (parse-validated-pos turn board)
            ) 
            ((not isEmpty)
                (print "Position is not empty. Put somewhere else")
                (terpri)
                (parse-validated-pos turn board)
            )
            (t 
                (list input-x input-y)
            )
        )
    )   
)

;; *********************************************************************
;; Function Name: print-stats-during-game
;; Purpose: Print game statistics and current state information.
;; Parameters:
;;   - hColor: The color of the human player.
;;   - cColor: The color of the computer player.
;;   - currentColor: The color of the player whose turn is next.
;;   - hCPScores: Human player's captured pairs score.
;;   - cCPScores: Computer player's captured pairs score.
;;   - hPrevscores: Human player's total score before this move.
;;   - cPrevscores: Computer player's total score before this move.
;; Return Value: None. This function prints the game statistics and information.
;; Algorithm:
;;   1. Print the color and statistics for the human player, including captured pairs and total score.
;;   2. Print the color and statistics for the computer player, including captured pairs and total score.
;;   3. Determine and print the color of the next player to make a move.
;; Assistance Received: None
;; *********************************************************************

(defun print-stats-during-game(hColor cColor currentColor hCPScores cCPScores hPrevscores cPrevscores)
    (princ "Human is ")
    (princ hColor)
    (princ " | Human Captured Pairs: ")
    (princ hCPScores)
    (princ " | Human Total Score: ")
    (princ hPrevscores)
    (terpri)
    (princ "Computer is ")
    (princ cColor)
    (princ " | Computer Captured Pairs: ")
    (princ cCPScores)
    (princ " | Computer Total Score: ")
    (princ cPrevscores)
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
(resume-round)