;; (defun save-string-to-file (filename data)
;;   (with-open-file (stream filename :direction :output
;;                                  :if-exists :supersede
;;                                  :if-does-not-exist :create)
;;     (format stream "~a" data)))

;; ; Example usage:
;; (save-string-to-file "output.txt" "Hello, this is a test.")

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
       (ask-to-save-game)))))

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

          ;; TODO call the save-to-file func here


          ;; for now print the game state
          (let* 
            (
              (board-cmnt 'Board)
              (human-cmnt 'Human)
              (computer-cmnt 'Computer)
              (next-player-cmnt 'NextPlayer)
              (game-state (list board-cmnt board human-cmnt hCPair hTotScore computer-cmnt cCPair CTotScore next-player-cmnt nextPlayer nextPlayerColor))
            )

            (print game-state)

            ;; return true to indicated end of the game
            t

          )
      )
    )
  )

  
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
       (ask-to-load-game)))))

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
      (hcPair (first (rest loaded-game)))
      (hTotScore (first (rest (rest loaded-game))))
      (cCpair (first (rest (rest (rest loaded-game)))))
      (cTotScore (first (rest (rest (rest (rest loaded-game))))))
      (nextPlayer (first (rest (rest (rest (rest (rest loaded-game)))))))
      (nextPlayerColor (first (rest (rest (rest (rest (rest (rest loaded-game))))))))
      (lastPlayerColor (cond 
                          ((equal nextPlayerColor (first '(W)))
                                (first '(B))
                          )
                          (t 
                                (first '(W))
                          )

      ))
      ;; colors[0] will be human colors[1] will be computer
      (colors (cond 
                ((equal nextPlayer 'Human)
                    (nextPlayerColor lastPlayerColor)
                )
                (t 
                    (lastPlayerColor nextPlayerColor)
                )

      ))
      (turn (determine-turn-number board hCPair cCPair))
    )

    ;; return the game state
    ;; board hColor cColor playingColor turn hCPScore cCPScore hFiveScore cFiveScore nextPlayer hTotScore cTotScore
    (board (first color) (first (rest color)) nextPlayerColor turn hCPair cCPair 0 0 nextPlayer hTotScore cTotScore)
  )
)

;; function to read data from the file


;; TODO
(defun fetch-game-stats (game-name)
  ()
)

(defun read-file (file-path)
  (with-open-file (stream file-path :direction :input)
    (loop for item = (read stream nil)
          until (eq item stream)
          collect item)))

;; Example usage:
(let ((file-path "game1.txt")) ; Replace with your file's path
  ( data (read-file file-path))
  (format t "Data from file: ~a~%" data))




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


;; ---------------------------------------------------------------------------------------------
;; TESTING

;; (let ((pente-board '(
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O B O W O O O O O O W O W W W W)
;;      ( O O O O O B O O O O O O O O W O W W W W)
;;      ( O O O O O B O O O O O O O O W O W W W W)
;;      ( O O O O O B O O O O O O O O W O W W W W)
;;      ( O O O O O B O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;      ( O O O O O O O O O O O O O O W O W W W W)
;;     )))

;;     (print (determine-turn-number pente-board 0 1))
;; )