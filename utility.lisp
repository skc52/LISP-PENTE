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
                ((equal (get-color board x y) color)
                    (consecutive board color (- count 1) (+ x dx) (+ y dy))
                )
                (t 
                    ()
                )
            )
           
        )
    )
)

;; (defun sum (line color count j)
;;     (cond 
;;         ((= j 20)
;;             0
;;         )
;;         (t 
;;             ;; consecutive of count?

;;         )
;;     )
;; )
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