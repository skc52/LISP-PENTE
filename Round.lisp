
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
       (ask-to-call)))))

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
        '(B W)
       (format t "You lost the call. Computer will start the game~%")))
    
   ))


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
    (playgame (update-board board  10 10 (first '(W))) (first colors) (first (rest colors)) (first '(B)) 1 0 0 0 0)

    ;; determine 4 cons
    ;; calculate total scores for both
    ;; display the scores
    ;; ask user if he wants to continue playing
    ;; if yes recall this function
    ;; else ()
    )
)

(defun playgame (board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore)
    ;; ask user input
    ;; validate input
    ;; parse input
    ;; check if input is empty
    ;; if empty put the piece in the position
    ;; pass the turn
    (print "---------------------------------------------------------------------------------------")
    (terpri)
    (print-board board)
    (terpri)
    (print-stats-during-game hColor cColor playingColor hCPScore cCPScore)
    (print "---------------------------------------------------------------------------------------")
    (terpri)

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

            ((and (equal turn 2) (not (or (or (<= 4  (- input-x 10) 18) (<= -18 (- input-x 10) -4)) (or (<= 4  (- input-y 10) 18) (<= -18 (- input-y 10) -4)))) ) 
                (print "Cannot put within 3 steps in 2nd turn of White. Enter new position")
                (terpri)
                (playGame board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore)
            ) 
            ((not isEmpty)
                (print "Position is not empty. Put somewhere else")
                (terpri)
                (playGame board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore)
            )
            ;; is empty then update the board
            (t 
                ;; check for captured pairs
                ;; check for 5 cons
                (playgame (update-board board  input-x input-y playingColor) hColor cColor nextColor (+ turn 1) hCPScore cCPScore hFiveScore cFiveScore)
            )
        )


    )
    

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
            (princ "Current Player: Human")
            (terpri)
        )
        (t 
            (princ "Current Player: Computer")
            (terpri)
        )
    )

)
(start-round 1 0)