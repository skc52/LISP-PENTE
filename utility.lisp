



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

;; 15 19 ox oy
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
                    (print "Row")
                            (print ox)
                    (sum-consecutive-backwardLower board color count (- ox 1) oy (- ox 1) oy  dx dy sum)
                )
                (t
                    (cond 
                    ;; if consecutive is true increment sum by 1 and x by 4 and y by -4
                        ((consecutive board color count x y dx dy)
                        
                            (sum-consecutive-backwardLower board color count ox oy (+ x 4) (- y 4) dx dy (+ sum 1))
                        )
                        ;; else increment x by 1 and y by -1 and continue looking
                        (t
                            (sum-consecutive-backwardLower board color count ox oy (+ x 1) (- y 1) dx dy sum)
                        )
                    )
                )
            )
        )
        
    )
)

;; 0 18 ox oy
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

(defun total-four-consecutive (pente-board color)
    (+ (sum-consecutive-forwardUpper pente-board (first '(W)) 4 0 18 0 18 1 -1 0) (
        + (sum-consecutive-forwardLower pente-board (first '(W)) 4 15 19 15 19 1 -1 0) (
            + (sum-consecutive-backwardUpper pente-board (first '(W)) 4 0 2 0 2 1 1 0) (
                + (sum-consecutive-backwardLower pente-board (first '(W)) 4 15 1 15 1 1 1 0) (
                    + (sum-consecutive-row pente-board (first '(W)) 4 0 1 0 1 0) (sum-consecutive-col pente-board (first '(W)) 4 0 1 1 0 0)
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
(defun capture-pair (board x y dx dy own-color opponent-color)
    ;; in the given direction check if the next two are opponent-color and the third is own
    ;; also do bound checkinh
            

    (cond
        
        ((not (and (<= 0 (+ x (* dx 3)) 18) (<= 1 (+ y (* dy 3)) 19) ))
            ()
        )
        ( (and (check-pair board x y dx dy own-color opponent-color) (equal (get-color board (+ x (* dx 3)) (+ y (* dy 3))) own-color) )
            '((dx dy))
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

