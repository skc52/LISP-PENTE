;; this file will have all utility functions

;; (+ (check-horizontal board piece-color 4) (+ (check-vertical board piece-color 4)  (+ (check-backward board piece-color 4)  (check-forward board piece-color 4) )))

;; j is column indexing,i is the row indexing
;; return 1 if conseuctive is possible from initial i,j for the color
(defun consecutive (board color count x y dx dy)
   (cond

        ((not (and (<= 0 x 18) (<= 1 y 19)))
            (print "Out of bound")
            (terpri)
            ()
        )

        (t
            (cond
                ((= count 0)
                    t   
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
;; ;; count is the number of consecutives we are looking for
;; (defun check-horizontal (board color count i)
;;     (cond
;;         ((= i 19)
;;             0
;;         )
;;         (t
;;             (+ (sum (first board) color count 1) (check-horizontal (rest board) color count (+ i 1)))
;;         )

;;     )
    
;; )

;; (defun check-vertical (board color count)

;; )
;; (defun check-backward (board color count)

;; )
;; (defun check-forward (board color count)

;; )