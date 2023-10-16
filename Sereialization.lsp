
;; /* *********************************************************************
;;    Function Name: ask-to-save-game
;;    Purpose: To prompt the user for saving the game and retrieve the filename.
;;    Return Value: Returns the filename to save to if the user chooses to save, or nil if the user chooses not to save.
;;    Algorithm:
;;        1) Display a message to the user to inquire about saving the game.
;;        2) Read the user's input (0 for 'no' or 1 for 'yes').
;;        3) If the user chooses 'no', return nil.
;;        4) If the user chooses 'yes', prompt the user to enter a filename and return the filename after saving.
;;        5) If the user provides an invalid input, display an error message and recursively call the function.
;;    Assistance Received: none
;; ********************************************************************* */
(defun ask-to-save-game ()
  (format t "Do you want to save the game? (Enter 0 for 'no' or 1 for 'yes')~%")
  (let ((user-input (read-line)))
    (cond 
      ((string= user-input "0")
       
       nil)
      ((string= user-input "1")
       (format t "Enter the filename to save the game:~%")
       (let ((filename (read-line)))
         (format t "Game saved as ~A~%" filename)
         filename))
      (t 
       (format t "Invalid input. Please enter '0' for 'no' or '1' for 'yes'.~%")
       (ask-to-save-game))))
)


;; /* *********************************************************************
;;    Function Name: save-game-to-file
;;    Purpose: To save game data to a file.
;;    Parameters:
;;        filename, a string representing the filename to save to
;;        data, the game data to be saved
;;    Algorithm:
;;        1) Open the specified file for output and create it if it does not exist.
;;        2) Write the game data to the file.
;;        3) Display a success message after saving.
;;    Assistance Received: none
;; ********************************************************************* */
(defun save-game-to-file(filename data)
  
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (print data stream))
    (princ "Game saved successfully!")
    (terpri)
)

;; /* *********************************************************************
;;    Function Name: save-game
;;    Purpose: To save the current game state to a file if the user chooses to save.
;;    Parameters:
;;        board, a list representing the game board
;;        hCPair, a list representing human player's capture pairs
;;        hTotScore, an integer representing human player's total score
;;        cCPair, a list representing computer player's capture pairs
;;        cTotScore, an integer representing computer player's total score
;;        nextPlayer, a symbol representing the next player to move
;;        nextPlayerColor, a character representing the color of the next player
;;    Return Value: Returns true after successfully saving the game state to a file.
;;    Algorithm:
;;        1) Prompt the user if they want to save the game using ask-to-save-game.
;;        2) If the user chooses not to save, return nil.
;;        3) If the user chooses to save, create a list of the game state without labels.
;;        4) Save the game state to the specified file.
;;        5) Return true to indicate the successful save.
;;    Assistance Received: none
;; ********************************************************************* */
(defun save-game (board hCPair hTotScore cCPair CTotScore nextPlayer nextPlayerColor)

  (let* 
    (
      (user-input (ask-to-save-game))
    )

    (cond 
      ((not user-input)
        ()
      )
      (t
          (let* 
            (
              (board-cmnt ";Board")
              (human-cmnt ";Human")
              (computer-cmnt ";Computer")
              (next-player-cmnt ";Next player")
              (nextPlayerColorName 
                    (cond 
                      ((equal (first '(W)) nextPlayerColor)
                          'White
                      )
                      (t 
                          'Black
                      )
                    )
              )

              (game-state (list (take-out-labels board)  hCPair hTotScore  cCPair CTotScore  nextPlayer nextPlayerColorName))

            )
            (save-game-to-file user-input game-state)

            ;; return true to indicate end of the game
            t

          )
      )
    )
  )
)


;; /* *********************************************************************
;;    Function Name: fetch-game-stats
;;    Purpose: To fetch and load game data from a specified file.
;;    Parameters:
;;        game-name, a string representing the filename from which to load the game data.
;;    Return Value: Returns the loaded game data.
;;    Algorithm:
;;        1) Open the specified file for input.
;;        2) Read and load the game data from the file.
;;        3) Display a success message after loading.
;;    Assistance Received: none
;; ********************************************************************* */

(defun fetch-game-stats (game-name)
  (with-open-file (stream game-name
                        :direction :input
                        :if-exists :error)
    (let ((data (read stream)))
      (format t "Game loaded successfully!~%")
      data))
)

;; /* *********************************************************************
;;    Function Name: scan-row
;;    Purpose: To scan through a row and return the total count of pieces up to that row.
;;    Parameters:
;;        row, a list representing a row of the game board
;;        count, an integer representing the count of pieces up to the current row
;;    Return Value: Returns the total count of pieces up to the given row.
;;    Algorithm:
;;        1) If the row is empty, return the count.
;;        2) If the first element of the row is not an empty space, increment the count.
;;        3) Recursively call scan-row on the rest of the row.
;;    Assistance Received: none
;; ********************************************************************* */
(defun scan-row (row count)
  (cond 
        ((null row)
            count
        )
        (t 
            (cond 
              ((not (equal (first row) (first '(O))))
                (scan-row (rest row) (+ count 1))
              )
              (t 
                (scan-row (rest row) count)
              )
              
            )
            
        )
    )
)

;; /* *********************************************************************
;;    Function Name: determine-num-piece
;;    Purpose: To go through the game board and get the total count of pieces.
;;    Parameters:
;;        board, a list representing the game board
;;        count, an integer representing the count of pieces up to the current row
;;    Return Value: Returns the total count of pieces on the game board.
;;    Algorithm:
;;        1) If the board is empty, return the count.
;;        2) Call scan-row on the first row of the board to get the count of pieces up to that row.
;;        3) Recursively call determine-num-piece on the rest of the board.
;;    Assistance Received: none
;; ********************************************************************* */
(defun determine-num-piece (board count)
  (cond
        ((null board)
            count
        )
        (t 
        ;; scan row returns the count of pieces upto that row 
            (determine-num-piece (rest board) (scan-row (first board) count))
        )
    )
)


;; /* *********************************************************************
;;    Function Name: determine-turn-number
;;    Purpose: To determine the current turn number based on the game board and capture scores of both players.
;;    Parameters:
;;        board, a list representing the game board
;;        hCPScore, an integer representing the human player's capture score
;;        cCPScore, an integer representing the computer player's capture score
;;    Return Value: Returns the current turn number.
;;    Algorithm:
;;        1) Determine the total number of pieces on the game board.
;;        2) Multiply the human player's capture score by 2 and the computer player's capture score by 2.
;;        3) Add the total number of pieces, twice the human player's capture score, and twice the computer player's capture score.
;;    Assistance Received: none
;; ********************************************************************* */

(defun determine-turn-number (board hCPScore cCPScore)
    ;; a) count the total number of pieces in the board
    ;; b) multiply the hCPscore and cCPScore by 2 and add them
    ;; add a and b 

  (+ (determine-num-piece board 0) (* hCPScore 2) (* cCPScore 2))


)

;; /* *********************************************************************
;;    Function Name: ask-to-load-game
;;    Purpose: To ask the user if they want to load a saved game and to get the game filename.
;;    Parameters: None
;;    Return Value: Returns the filename if the user chooses to load a game, or nil if not.
;;    Algorithm:
;;        1) Prompt the user to enter 0 for 'no' or 1 for 'yes.'
;;        2) If the user enters 0, return nil.
;;        3) If the user enters 1, prompt for the game filename and return it.
;;        4) Handle invalid input by prompting the user again.
;;    Assistance Received: none
;; ********************************************************************* */
(defun ask-to-load-game ()
  (format t "Do you want to load the game? (Enter 0 for 'no' or 1 for 'yes')~%")
  (let ((user-input (read-line)))
    (cond 
      ((string= user-input "0")
       
       nil)
      ((string= user-input "1")
       (format t "Enter the filename to load the game from:~%")
       (let ((filename (read-line)))
         (format t "Loading game from ~A~%" filename)
         filename))
      (t 
       (format t "Invalid input. Please enter '0' for 'no' or '1' for 'yes'.~%")
       (ask-to-load-game))))
)

;; /* *********************************************************************
;;    Function Name: add-labels
;;    Purpose: To add row and column labels to the game board.
;;    Parameters:
;;        board, a list representing the game board without labels.
;;    Return Value: Returns the game board with row and column labels added.
;;    Algorithm:
;;        1) Create labeled rows and columns for the game board.
;;        2) Return the game board with labels.
;;    Assistance Received: none
;; ********************************************************************* */

(defun add-labels (board)
  (let* 
    (
      (row1 (cons 19 (first board)))
      (row2 (cons 18 (first (rest board))))
      (row3 (cons 17 (first (rest (rest board)))))
      (row4 (cons 16 (first (rest (rest (rest board))))))
      (row5 (cons 15 (first (rest (rest (rest (rest board)))))))
      (row6 (cons 14 (first (rest (rest (rest (rest (rest board))))))))
      (row7 (cons 13 (first (rest (rest (rest (rest (rest (rest board)))))))))
      (row8 (cons 12 (first (rest (rest (rest (rest (rest (rest (rest board))))))))))
      (row9 (cons 11 (first (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))
      (row10(cons 10 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))
      (row11 (cons 9 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))
      (row12 (cons 8 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))))
      (row13 (cons 7 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))
      (row14 (cons 6 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))))))
      (row15 (cons 5 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))))
      (row16 (cons 4 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))))))))
      (row17 (cons 3 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))))))
      (row18 (cons 2 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest  board))))))))))))))))))))
      (row19 (cons 1 (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))))))))
      (row20 (list 0 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P 'Q 'R 'S))
    )

    (list row1 row2 row3 row4 row5 row6 row7 row8 row9 row10 row11 row12 row13 row14 row15 row16 row17 row18 row19 row20)
  )
)

;; /* *********************************************************************
;;    Function Name: take-out-labels
;;    Purpose: To remove row and column labels from the game board.
;;    Parameters:
;;        board, a list representing the game board with labels.
;;    Return Value: Returns the game board without row and column labels.
;;    Algorithm:
;;        1) Remove row and column labels from the game board.
;;        2) Return the modified game board without labels.
;;    Assistance Received: none
;; ********************************************************************* */

(defun take-out-labels (board)
 (let* 
    (
      (row1 (rest (first board)))
      (row2 (rest (first (rest board))))
      (row3 (rest (first (rest (rest board)))))
      (row4 (rest (first (rest (rest (rest board))))))
      (row5 (rest (first (rest (rest (rest (rest board)))))))
      (row6 (rest (first (rest (rest (rest (rest (rest board))))))))
      (row7 (rest (first (rest (rest (rest (rest (rest (rest board)))))))))
      (row8 (rest (first (rest (rest (rest (rest (rest (rest (rest board))))))))))
      (row9 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))
      (row10(rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))
      (row11 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))
      (row12 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))))
      (row13 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))
      (row14 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))))))
      (row15 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))))
      (row16 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board))))))))))))))))))
      (row17 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))))))
      (row18 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest  board))))))))))))))))))))
      (row19 (rest (first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest board)))))))))))))))))))))
    )
    (list row1 row2 row3 row4 row5 row6 row7 row8 row9 row10 row11 row12 row13 row14 row15 row16 row17 row18 row19)

    )
)

;; /* *********************************************************************
;;    Function Name: load-game
;;    Purpose: To load a saved game state from a file and prepare it for resumption.
;;    Parameters:
;;        game-name, a string representing the filename from which to load the game state.
;;    Return Value: Returns the loaded game state.
;;    Algorithm:
;;        1) Fetch and load the game data from the specified file.
;;        2) Add row and column labels to the game board.
;;        3) Determine the player colors and turn number.
;;        4) Return the loaded game state with updated player colors and turn number.
;;    Assistance Received: none
;; ********************************************************************* */

(defun load-game (game-name)
  ;; after loading game i will have to determine colors for both players and turn number
  ;; note that for a new game turn num starts at 0 not 1

  ;; ask to load game

  (let* 
    (

      ;; board hCPair hTotScore cCPair CTotScore nextPlayer nextPlayerColor
      (loaded-game (fetch-game-stats game-name))
      (board (first loaded-game))
      (board (add-labels board))
      (hcPair (first (rest loaded-game)))
      (hTotScore (first (rest (rest loaded-game))))
      (cCpair (first (rest (rest (rest loaded-game)))))
      (cTotScore (first (rest (rest (rest (rest loaded-game))))))
      (nextPlayer (first (rest (rest (rest (rest (rest loaded-game)))))))
      (nextPlayerColor (first (rest (rest (rest (rest (rest (rest loaded-game))))))))
      (lastPlayerColor (cond 
                          ((equal nextPlayerColor (first '(WHITE)))

                                (first '(B))
                          )
                          (t 
                                (first '(W))
                          )

      ))

      (nextPlayerColor (cond 
                          ((equal lastPlayerColor (first '(W)))

                                (first '(B))
                          )
                          (t 
                                (first '(W))
                          )

      ))
      ;; colors[0] will be human colors[1] will be computer
      (colors (cond 
                ((equal nextPlayer (first '(Human)))
                    (list nextPlayerColor lastPlayerColor)
                )
                (t 
                    (list lastPlayerColor nextPlayerColor)
                )

      ))
      (turn (determine-turn-number board hCPair cCPair))
    )

    ;; return the game state
    ;; board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore nextPlayer hTotScore cTotScore
    (list board (first colors) (first (rest colors)) nextPlayerColor turn hCPair cCPair 0 0 nextPlayer hTotScore cTotScore)
  )
)
