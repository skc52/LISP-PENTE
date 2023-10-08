;; we are creating a board of priorities

(defun create-row-priority (n)
  (cond 
    ((= n 1)
     '(X A B C D E F G H I J K L M N O P Q R S))
    (t 
     (append (list (- n 1))
             (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))))

(defun create-board-priority (n)
  (cond 
    ((= n 0)
     nil)
    (t 
     (cons (create-row-priority n) (create-board-priority (- n 1))))))



;; Check Five consecutive possibility
    ;; if winning with vast difference, prioritize getting points by 4 consecutive or pairs capturing
    ;; else if losing and by 5 consecutive can win the game, maximum priority
    ;; else if losing by vast and ending the game cannot win,then low priority


;; go through each valid position in the board and determine its 5 consecutive
;; start from 0 1 and go upto 18 19 
;; pboard is priority board and board is gameboard
(defun set-priority-based-on-5-cons(pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            pboard
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-5-cons pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-5-cons pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                    (newPriority (length (five-consecutive board x y ownColor)))
                )

                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-5-cons (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-5-cons pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)

(defun set-priority-based-on-4-cons(pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            pboard
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-4-cons pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-4-cons pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                    (newPriority (length (four-consecutive board x y ownColor)))
                )
                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-4-cons (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-4-cons pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)

(defun set-priority-based-on-3-cons(pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            pboard
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-3-cons pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-3-cons pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
            ;; then update the corresponding priority in p board accordingly
            ;; then call this function on the next position
            ;; compare the new calculated priority with the previous priority
            ;; if new is greater update

            (let* 
                (
                    (newPriority (length (three-consecutive board x y ownColor)))
                )

                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-3-cons (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-3-cons pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)

;; complementary functions will be checking open ends, if so adding to the priority








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

;; check-and-capture-pairs
(defun set-priority-based-on-pairs-captured(pboard board x y ownColor)
    (cond
        ;; if reached the last of the row and the end of the col
        ((and (equal y 20) (equal x 18))
            pboard
        )
        ;; if reached the end of the column, increment row
        ((equal y 20)
            (set-priority-based-on-pairs-captured pboard board (+ x 1) 1 ownColor)
        )
        ;; ignore filled cells in the board
        ((not (equal (get-color board x y) (first '(O))))
            (set-priority-based-on-pairs-captured pboard board x (+ y 1) ownColor)

        )
        (t  
            ;;  determine 5-cons possible priorities    
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
                    (newPriority (length (check-and-capture-pairs board x y ownColor opponentColor)))
                )

                (cond 
                    ((> newPriority (get-color pboard x y))
                        (set-priority-based-on-pairs-captured (update-board pboard x y newPriority) board x (+ y 1) ownColor)

                    )
                    (t
                        (set-priority-based-on-pairs-captured pboard board x (+ y 1) ownColor)

                    )
                )
            
            )
        )
    )

)