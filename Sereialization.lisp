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

          ;; call the save-to-file func here


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