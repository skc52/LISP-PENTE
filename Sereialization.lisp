
;; returns the filename to save to or nil
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


;; saves data to file
(defun save-game-to-file(filename data)
  
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (print data stream))
    (princ "Game saved successfully!")
    (terpri)
)

;; returns true after saving game stats to file
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
              ;; TODO take out board labels

              ;; IMPORTANT i commented this out because i could not include comments as expected in the gamefile
              ;; (game-state (list board-cmnt (take-out-labels board) human-cmnt hCPair hTotScore computer-cmnt cCPair CTotScore next-player-cmnt nextPlayer nextPlayerColorName))
              (game-state (list (take-out-labels board)  hCPair hTotScore  cCPair CTotScore  nextPlayer nextPlayerColorName))

            )

            ;; (print game-state)

            (save-game-to-file user-input game-state)

            ;; return true to indicate end of the game
            t

          )
      )
    )
  )
)


(defun fetch-game-stats (game-name)
  (with-open-file (stream game-name
                        :direction :input
                        :if-exists :error)
    (let ((data (read stream)))
      (format t "Game loaded successfully!~%")
      data))
)

;; will scan through a row and return the total count of pieces upto that row
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

;; go through the board and get the total count of pieces
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

(defun determine-turn-number (board hCPScore cCPScore)
    ;; a) count the total number of pieces in the board
    ;; b) multiply the hCPscore and cCPScore by 2 and add them
    ;; add a and b 

  (+ (determine-num-piece board 0) (* hCPScore 2) (* cCPScore 2))


)

;; ask to load game
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

;; will return the game state
;; board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore nextPlayer hTotScore cTotScore
;; hFiveScore cFiveScore will be 0
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



;; (print (fetch-game-stats "game5"))