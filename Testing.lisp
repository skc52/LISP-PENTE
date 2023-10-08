(let ((pente-board '(
    ( O O O O O O O O O O O O O O W O W W W W)
    ( O O O O O O O O O O O O O O W O W W W W)
    ( O O O O O O O O O O O O O O W O W W W W)
    ( O O O O O O O O O O O O O O W O W W W W)
    ( O O O O O O O O O O O O O O W O W W W W)
    ( O O O W O O O O O O O O O O W O W W W W)
    ( O O O W O O O O O O O O O O W O W W W W)
    ( O O O W O O O O O O O O O O W O W W W W)
    ( O O O W O O O O O O O O O O W O W W W W)
    ( O O O O W W W W W O O O O O W O W W W W)
    ( O O O W O B O O O O O O O O W O W W W W)
    ( O O O W O B O O O O O O O O W O W W W W)
    ( O O O W O B O O O O O O O O W O W W W W)
    ( O O O W O B O O W O O W O O W O W W W W)
    ( O O O O O B O O B O B O O O W O W W W W)
    ( O W O O O O O O B B O O W O W O W W W W)
    ( O B O O O W B B O B B W B O W O W W W W)
    ( O B O O O O O O O O O O B O W O W W W W)
    ( O O B B W O O O O W B B O B B W W W W W)
    ( O O B B W O O O O O O O O O W O W W W W)

    )))
   
;; (print-board (create-board-priority 20))

;; (defun set-priority-based-on-5-cons(pboard board x y ownColor)

    ;; (print-board (set-priority-based-on-5-cons (create-board-priority 20) pente-board 0 1 (first '(W))))
    (print-board (set-priority-based-on-pairs-captured (create-board-priority 20) pente-board 0 1 (first '(W))))


)