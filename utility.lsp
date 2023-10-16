

(load "Board.lsp")

;; this file will have all utility functions

;; (+ (check-horizontal board piece-color 4) (+ (check-vertical board piece-color 4)  (+ (check-backward board piece-color 4)  (check-forward board piece-color 4) )))

;; j is column indexing,i is the row indexing
;; return 1 if conseuctive is possible from initial i,j for the color
(defun consecutive (board color count x y dx dy)
   (cond

        ((= count 0)
            t   
        )
        

        (t
            (cond
                ((not (and (<= 0 x 18) (<= 1 y 19)))
                    
                    ()
                )
                (t
                    (cond 
                        ((equal (get-color board x y) color)
                            (consecutive board color (- count 1) (+ x dx) (+ y dy) dx dy)
                        )
                        (t 

                            ()

                        )
                    )
                )
            
            )
            
        )
        
    )
)

(defun sum-consecutive-row (board color count x y dx dy sum)
    (cond 
 
        ((> x 18)
        ;; return the sum
            sum

        )
        (t 
            (cond 
                ((> y 16)
                    ;; start looking at new row
                    (sum-consecutive-row board color count (+ x 1) 1 dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and y by 4
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-row board color count x (+ y 4) dx dy (+ sum 1))
                        )
                        ;; else increment y by 1 and continue looking
                        (t
                            (sum-consecutive-row board color count x (+ y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

(defun sum-consecutive-col (board color count x y dx dy sum)
    (cond 
 
        ((> y 19)
        ;; return the sum
            sum

        )
        (t 
            (cond 
                ((> x 15)
                    ;; start looking at new col
                    (sum-consecutive-col board color count 0 (+ y 1) dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-col board color count (+ x 4) y dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and continue looking
                        (t
                            (sum-consecutive-col board color count (+ x 1) y dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)


(defun sum-consecutive-backwardLower (board color count ox oy x y dx dy sum)
    (cond 
 
        ((< ox 0)
        ;; return the sum
            sum

        )
        (t 
            (cond 
                ((> x 18)
                    ;; start looking at new col
            
                    (sum-consecutive-backwardLower board color count (- ox 1) oy (- ox 1) oy  dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by 4
                        ((consecutive board color count x y dx dy)
                        
                            (sum-consecutive-backwardLower board color count ox oy (+ x 4) (+ y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by 1 and continue looking
                        (t
                            (sum-consecutive-backwardLower board color count ox oy (+ x 1) (+ y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

(defun sum-consecutive-backwardUpper (board color count ox oy x y dx dy sum)
    (cond 
        
        ((> oy 16)
        ;; return the sum
            sum

        )
        (t 
            
            (cond 
                ((> y 19)
                    ;; start looking at new col
                    (sum-consecutive-backwardUpper board color count ox (+ oy 1) ox (+ oy 1) dx dy sum)
                )
                (t
                   (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by 4
                    
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-backwardUpper board color count ox oy (+ x 4) (+ y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by 1 and continue looking
                        (t
                            (sum-consecutive-backwardUpper board color count ox oy (+ x 1) (+ y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; 15 19 ox oyoriginX originY of the diagonal search
;; 1 -1 dx dy
(defun sum-consecutive-forwardLower (board color count ox oy x y dx dy sum)
    (cond 
 
        ((< ox 0)
        ;; return the sum
            sum
        )
        (t 
            (cond 
                ((> x 18)
                    ;; start looking at new col
                    ;; (print "Row")
                    ;;         (print ox)
                    (sum-consecutive-forwardLower board color count (- ox 1) oy (- ox 1) oy  dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by -4
                        ((consecutive board color count x y dx dy)
                        
                            (sum-consecutive-forwardLower board color count ox oy (+ x 4) (- y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by -1 and continue looking
                        (t
                            (sum-consecutive-forwardLower board color count ox oy (+ x 1) (- y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; 0 18 => ox oy originX originY of the diagonal search
;; 1 -1 dx dy
(defun sum-consecutive-forwardUpper (board color count ox oy x y dx dy sum)
    (cond 
 
        ((< oy 4)
        ;; return the sum
            sum
        )
        (t 
            (cond 
                ((< y 1)
                    ;; start looking at new col
                    (sum-consecutive-forwardUpper board color count ox (- oy 1) ox (- oy 1) dx dy sum)
                )
                (t
                   (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by 4
                        ((consecutive board color count x y dx dy)
                            (sum-consecutive-forwardUpper board color count ox oy (+ x 4) (- y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by 1 and continue looking
                        (t
                            (sum-consecutive-forwardUpper board color count ox oy (+ x 1) (- y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; this function will be used to determine the total number of 4 consecutives once the round is over
(defun total-four-consecutive (pente-board color)
    (+ (sum-consecutive-forwardUpper pente-board color 4 0 18 0 18 1 -1 0) (
        + (sum-consecutive-forwardLower pente-board color 4 15 19 15 19 1 -1 0) (
            + (sum-consecutive-backwardUpper pente-board color 4 0 2 0 2 1 1 0) (
                + (sum-consecutive-backwardLower pente-board color 4 15 1 15 1 1 1 0) (
                    + (sum-consecutive-row pente-board color 4 0 1 0 1 0) (sum-consecutive-col pente-board color 4 0 1 1 0 0)
                )
            )
        )
    )) 
)

;returns t if true
(defun check-pair (board x y dx dy own-color opponent-color)
    ;; in the given direction check if the next two are opponent-color
    ;; also do bound checkinh
    (cond
        ((not (and (<= 0 (+ x dx) 18) (<= 1 (+ y dy) 19) ))
            ()
        )
         ((not (and (<= 0 (+ x (* dx 2)) 18) (<= 1 (+ y (* dy 2)) 19) ))
            ()
        )
        ( (and (equal (get-color board (+ x dx) (+ y dy)) opponent-color) (equal (get-color board (+ x (* dx 2)) (+ y (* dy 2))) opponent-color))
            t
        )
        (t 
            ()
        )
    )
)

; will return direction if opponent pairs are present in the given direction
;; make sure return in thr form ((dx, dy))
;; so when appending in check-pairs (dx,dy) can be still a unit

;; The reason i did not return reason from this function like for consecutives is that this function is being used
;; in actually capturing pairs and not just determining the possibility
(defun capture-pair (board x y dx dy own-color opponent-color)
    ;; in the given direction check if the next two are opponent-color and the third is own
    ;; also do bound checkinh
            

    (cond
        
        ((not (and (<= 0 (+ x (* dx 3)) 18) (<= 1 (+ y (* dy 3)) 19) ))
            ()
        )
        ( (and (check-pair board x y dx dy own-color opponent-color) (equal (get-color board (+ x (* dx 3)) (+ y (* dy 3))) own-color) )
            (list (list dx dy))
        )
        (t 
            ()
        )
    )
)


;; now check for pairs to capture for a particular row and column
;; return an array of directions indicating pairs to capture
;; check in every direction , 8 directions
(defun check-and-capture-pairs (board x y  own-color opponent-color)
    (append (capture-pair board x  y 0 1 own-color opponent-color) (
        append (capture-pair board x  y 0 -1 own-color opponent-color) (
            append (capture-pair board x  y 1 0 own-color opponent-color) (
                append (capture-pair board x  y -1 0 own-color opponent-color) (
                    append (capture-pair board x  y 1 1 own-color opponent-color) (
                        append (capture-pair board x  y -1 -1 own-color opponent-color) (
                            append (capture-pair board x  y 1 -1 own-color opponent-color) (capture-pair board x  y -1 1 own-color opponent-color)
                        )
                    )
                )
            )
        )
    ) )
)

;; get the sum of capture counts for a particular x and y
;; it is gonna be the lenth of the list of directions returned by check and capture
(defun determine-capture-count (board x y own-color opponent-color)
    (length (check-and-capture-pairs board x y own-color opponent-color))

)


;; function to determine counsecutive count in a direction
(defun determine-consecutive-count (board x y dx dy color ctr)

    (cond 
        ((not (and (<= 0 (+ x dx) 18) (<= 1 (+ y dy) 19)))
            (list ctr)
        )
        ((equal (get-color board (+ x dx) (+ y dy)) color) 
            (determine-consecutive-count board (+ x dx) (+ y dy) dx dy color (+ ctr 1))
        )
        (t 
            (list ctr)
        )
    )
)

;; emptyRequiredCnt is the minimum empty count required to be feasible for 4
(defun determine-empty-count (board x y dx dy color ctr emptyRequiredCnt)

    (cond 
        ((equal emptyRequiredCnt 0)
            (list ctr)
        )
        ((not (and (<= 0 (+ x dx) 18) (<= 1 (+ y dy) 19)))
            (list ctr)
        )
        ((equal (get-color board (+ x dx) (+ y dy)) color) 
            (determine-empty-count board (+ x dx) (+ y dy) dx dy color (+ ctr 1) (- emptyRequiredCnt 1))
        )
        (t 
            (list ctr)
        )
    )
)

;; function to determine open ends
(defun determine-open-ends (board x y dx dy color ctr consCount emptyRequiredCnt)

    (let* 
        (
            ;; openCount is the number of open positions right after the consecutive ones
            (openCount (first  (determine-empty-count board x y (* dx (+ consCount 1)) (* dy (+ consCount 1)) (first '(O)) 0 emptyRequiredCnt)))
        )

        (list openCount)
    )
)

;; returns 1 and "Five consecutive in " directionname for a particular direction
(defun five-possible (board x y dx dy color direction-name)
    (let* (
        (consecutive-ctr-left (first (determine-consecutive-count board x y dx dy color 0)))
        (consecutive-ctr-right (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
        (left (cond 
            ((>= consecutive-ctr-left 4)
                1
            )
            (t 
                0
            )
        ))
        (right (cond 
            ((>= consecutive-ctr-right 4)
                1
            )
            (t 
                0
            )
        ))
            
        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
        )

        

    (cond 
            ((>= consecutive-ctr  4)

            
                (list 1 (format nil "Five Consecutive in ~A. " direction-name))
            )
            (t 
                (list 0 "")
            )

    )
    )
)


;; returns the toral number of five consecutives for a position and also the reasons
(defun five-consecutive (board x y color)
   (let 
    (
        (fiveConsCount (+ (first (five-possible board x y 0 1 color "horizontal")) (
        + (first (five-possible board x y 1 0 color "vertical")) (
            + (first (five-possible board x y 1 1 color "backward diagonal")) (first (five-possible board x y 1 -1 color "forward diagonal"))
            )
   ) ))

    (reason (format nil "~A~A~A~A" (first (rest (five-possible board x y 0 1 color "horizontal") )) 
            (first (rest (five-possible board x y 1 0 color "vertical")))
            (first (rest (five-possible board x y 1 1 color "backward diagonal")))
            (first (rest (five-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )
    (list fiveConsCount reason)
   )
)


;; using the concept from above
;; function to determine if in a given position and in the given direction, if 4 cons is possible if i place
;; my piece there
;; if it is possible return a list (left , right)
;; where left == 1 means that 4 consecutive is possible in left part of the given direction
;; and right == 1 means that it is possible in the right part of the given direction

;; (defun four-possible (board x y dx dy color)
;;     (let* (
;;             (consecutive-ctr-right (first (determine-consecutive-count board x y dx dy color 0)))
;;             (right-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-right 1)))
;;             (consecutive-ctr-left (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
;;             (left-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-left 1)))

;;         (left (cond 
;;             ((>= consecutive-ctr-left 3)
;;                 1
;;             )
;;             (t 
;;                 0
;;             )
;;         ))
;;         (right (cond 
;;             ((>= consecutive-ctr-right 3)
;;                 1
;;             )
;;             (t 
;;                 0
;;             )
;;         ))
;;         (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 

;;         ;; (priority-based-on-4cons (cond 
;;         ;;         ((= consecutive-ctr 3)

;;         ;;         )
;;         ;;         ;; tessera formation
;;         ;;         ((and (> right-open-count 0) (> left-open-count 0))
;;         ;;             35
;;         ;;         )
;;         ;;         ((or (> left-open-count 0) (> right-open-count 0) )
;;         ;;             15
;;         ;;         )
;;         ;;         (t 
;;         ;;             12
;;         ;;         )

;;         ;; ))
            
;;         )
;;     (cond 
;;             ((= consecutive-ctr  3)
;;                 (cond 
;;         ;;       
;;                     ;; tessera formation
;;                     ((and (> right-open-count 0) (> left-open-count 0))
;;                         35
;;                     )
;;                     ((or (> left-open-count 0) (> right-open-count 0) )
;;                         15
;;                     )
;;                     (t 
;;                         12
;;                     )

;;                 )
            
;;                 ;; (list (list left right dx dy))
;;             )
;;             (t 
;;                 0
;;             )

;;     )
;;     )
;; )


;; returns priority for x y for a particular direction for 4cons
(defun four-possible (board x y dx dy color direction-name)
    (let* (
            (consecutive-ctr-right (first (determine-consecutive-count board x y dx dy color 0)))
            (right-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-right 1)))
            (consecutive-ctr-left (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
            (left-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-left 1)))

        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
            
        )
    (cond 
            ((= consecutive-ctr  3)
                (cond 
                    ;; tessera formation
                    ((and (> right-open-count 0) (> left-open-count 0))
                        (list 35 (format nil "Both side open four consecutive - ~A. " direction-name))
                    )
                    ((or (> left-open-count 0) (> right-open-count 0) )
                        (list 15 (format nil "One side open Four Consecutive ~A. " direction-name ))
                    )
                    (t 
                       (list 12 (format nil "Four Consecutive ~A. " direction-name))
                    )

                   )    
            )
            (t 
                (list 0 "")
            )

    )
    )
)

;; returns the priority for x y position based on 4cons
(defun four-consecutive (board x y color)
   (let 
    (
        (priority (+ (first (four-possible board x y 0 1 color "horizontal")) (
                    + (first (four-possible board x y 1 0 color "vertical")) (
                        + (first (four-possible board x y 1 1 color "backward diagonal")) (first (four-possible board x y 1 -1 color "forward diagonal") )
                        )
            ) ))

        (reason (format nil "~A~A~A~A" (first (rest (four-possible board x y 0 1 color "horizontal") )) 
            (first (rest (four-possible board x y 1 0 color "vertical")))
            (first (rest (four-possible board x y 1 1 color "backward diagonal")))
            (first (rest (four-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )

    (list priority reason)
   
   )
)



;; returns priority for x y for a particular direction for 3cons
(defun three-possible (board x y dx dy color direction-name)
    (let* (
            (consecutive-ctr-right (first (determine-consecutive-count board x y dx dy color 0)))
            (right-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-right 1)))
            (consecutive-ctr-left (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
            (left-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-left 1)))

        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
            
        )

    ;; (print consecutive-ctr)
    (cond 
            ((= consecutive-ctr  2)
                (cond 
                    ;; tessera formation
                    ((and (> right-open-count 0) (> left-open-count 0))
                        (list 9 (format nil "Both side open three consecutive - ~A. " direction-name))
                    )
                    ((or (> left-open-count 0) (> right-open-count 0) )
                        (list 6 (format nil "One side open three Consecutive ~A. " direction-name ))
                    )
                    (t 
                       (list 0 (format nil "" ))
                    )

                   )    
            )
            (t 
                
                (list 0 "")
            )

    )
    )
)

;; returns the priority for x y position based on 4cons
(defun three-consecutive (board x y color)
   (let 
    (
        (priority (+ (first (three-possible board x y 0 1 color "horizontal")) (
                    + (first (three-possible board x y 1 0 color "vertical")) (
                        + (first (three-possible board x y 1 1 color "backward diagonal")) (first (three-possible board x y 1 -1 color "forward diagonal") )
                        )
            ) ))

        (reason (format nil "~A~A~A~A" (first (rest (three-possible board x y 0 1 color "horizontal") )) 
            (first (rest (three-possible board x y 1 0 color "vertical")))
            (first (rest (three-possible board x y 1 1 color "backward diagonal")))
            (first (rest (three-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )

    (list priority reason)
   
   )
)


;; ;; returns priority for x y for a particular direction for 3cons
;; (defun three-possible (board x y dx dy color)
;;     (let* (
;;             (consecutive-ctr-right (first (determine-consecutive-count board x y dx dy color 0)))
;;             (right-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-right 1)))
;;             (consecutive-ctr-left (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
;;             (left-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-left 1)))

;;         (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 
            
;;         )
;;     (cond 
;;             ((= consecutive-ctr  2)
;;                 (cond 
;;                     ;; lead to tessera formation
;;                     ((and (> right-open-count 0) (> left-open-count 0))
;;                         10
;;                     )
;;                     ((or (> left-open-count 0) (> right-open-count 0) )
;;                         6
;;                     )
;;                     (t 
;;                         0
;;                     )

;;                    )    
;;             )
;;             (t 
;;                 0
;;             )

;;     )
;;     )
;; )
;; ;; returns the priority for x y position based on 3cons
;; (defun three-consecutive (board x y color)
;;    (+ (three-possible board x y 0 1 color) (
;;         + (three-possible board x y 1 0 color) (
;;             + (three-possible board x y 1 1 color) (three-possible board x y 1 -1 color)
                
;;             )
;;         )
;;    ) 
;; )


;; TODO UNIT TEST
;; only return if both ends are open and have chance of creating 4
(defun two-possible (board x y dx dy color directionname)
    (let* (
        (enemyColor (cond 
            ((equal (first '(W)) color)
                (first '(B))
            )
            (t
                (first '(W))
            )

        ))
        (consecutive-ctr-left (first (determine-consecutive-count board x y dx dy color 0)))
        (left-open-count (first (determine-open-ends board x y dx dy color 0 consecutive-ctr-left 2)))
        (rigtNbrEnemy (cond 
                ;; (+ x (* dx -1)) is giving me error
                ((equal enemyColor (get-color board (+ x (* dx -1)) (+ y (* dy -1))))
                    t
                )
                (t 
                    ()
                )
            ))
        (consecutive-ctr-right (first (determine-consecutive-count board x y (* dx -1) (* dy -1) color 0)))
        (right-open-count (first (determine-open-ends board x y (* dx -1) (* dy -1) color 0 consecutive-ctr-right 2)))
        (leftNbrEnemy (cond 
                ((equal enemyColor (get-color board (+ x dx) (+ y dy)))
                    t
                )
                (t 
                    ()
                )
            ))
        (left (cond 
            ((>= consecutive-ctr-left 1)
                1
            )
            (t 
                0
            )
        ))
        (right (cond 
            ((>= consecutive-ctr-right 1)
                1
            )
            (t 
                0
            )
        ))
            
        (consecutive-ctr (+  consecutive-ctr-left consecutive-ctr-right)) 


        )

        
            ;; (print leftNbrEnemy)
            ;; (print rigtNbrEnemy)

   
    (cond 
            ;;TODO FIX THIS if chances of capture, dont return the list
            ;; the following code considers the case Enemy EMpty Own
            ;; Dont place in Empty
            ((and (equal consecutive-ctr 1) (or leftNbrEnemy rigtNbrEnemy) ) 
                
                ;; ()
                (list 0 "")
            )

            ;; for the case
            ;; ENemy Own EMpty, dont place in empty
            ((and (equal consecutive-ctr-left 1) (and (equal consecutive-ctr-right 0) (equal left-open-count 0) ))
                ;; ()
                (list 0 "")
            )
             ((and (equal consecutive-ctr-right 1) (and (equal consecutive-ctr-left 0) (equal right-open-count 0) ))
                ;; ()
                (list 0 "")
            )


            ((and (>= consecutive-ctr  1) (or (> left-open-count 1) (> right-open-count 1) )) 

            
                ;; (list (list left right dx dy))
                (list 2 (format nil "Two consecutives in ~A direction " directionname))
            )
            (t 
                ;; ()
                (list 0 "")
            )

    )
    )
)

(defun two-consecutive (board x y color)
   (let 
    (
        (priority (+ (first (two-possible board x y 0 1 color "horizontal")) (
                    + (first (two-possible board x y 1 0 color "vertical")) (
                        + (first (two-possible board x y 1 1 color "backward diagonal")) (first (two-possible board x y 1 -1 color "forward diagonal") )
                        )
            ) ))

        (reason (format nil "~A~A~A~A" (first (rest (two-possible board x y 0 1 color "horizontal") )) 
            (first (rest (two-possible board x y 1 0 color "vertical")))
            (first (rest (two-possible board x y 1 1 color "backward diagonal")))
            (first (rest (two-possible board x y 1 -1 color "forward diagonal")))
        
        ))
    )

    (list priority reason)
   
   )
)




;; TEsting

(let* 
    (
        (board 
          '((O W O W W B O O O O O B O O B O O O O O)
            (O O O O O O O O O O B O O O O B O O O O)
            (O O O O O O O O O B O O O O O O B O O O)
            (O O O O O O O O B O O O O O O O O B O O)
            (O O O B O O O O O O O O O O O O B O O O)
            (O O B O O B B O B B O O O O O B O O O O)
            (O O O O O O O O O O O O O O B O O O O O)
            (O O O O O O O O O B O O O O O O O O O O)
            (B O O O O O O O O O B O O O O O O O O O)
            (O O O B O W W B O O O B O O O O O O O O)
            (O O O O O O O O O O O O B O O O O O O O)
            (O B O O O O O O B O O O O O O O O O O O)
            (O B O O O O O O O O O O O O O O O O O O)
            (O O O O W W W W O W W W W O O O O O O O)
            (O O O O O O O O O O O O O B O O O W O O)
            (O B O O O O O O O O O O O O O O O O O O)
            (O O O O O O O O O O O O O O O O O W O O)
            (O B O O O O O O O O O O O O O O O O O O)
            (O O O O O O O O O O O O O O O O O O O O))
        )
    )


    ;; (print-board board)
    ;; (print (sum-consecutive-forwardUpper board (first '(B)) 4 0 18 0 18 1 -1 0) )
    ;; (print (sum-consecutive-forwardLower board (first '(B)) 4 15 19 15 19 1 -1 0))
    ;; (print (sum-consecutive-backwardUpper board (first '(B))  4 0 2 0 2 1 1 0) )
    ;; (print (sum-consecutive-backwardLower board (first '(B)) 4 15 1 15 1 1 1 0) )

    ;; (print (consecutive board (first '(B)) 4 3 17 1 -1))
    ;; (print (consecutive board (first '(B)) 4 0 14 1 1))
    

    ;; (print (four-possible board 0 2 0 1 (first '(W)) "horizonat"))

)