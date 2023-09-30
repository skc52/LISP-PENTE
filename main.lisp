;; Welcome message
(print "Welcome to pente!!!")
(format t "~%")

(defun ask-to-call ()
  (format t "We are tossing a coin to determine who starts the round. What would you like to call?~%")
  (format t "Enter 0 for Heads, 1 for Tails~%")

  (let ((user-choice (read-line)))
    (cond 
      ((string= user-choice "0")
       (format t "You entered Heads~%")
       0)
      ((string= user-choice "1")
       (format t "You entered Tails~%")
       1)
      (t 
       (format t "Invalid input. Please enter 0 for heads or 1 for tails~%")
       (ask-to-call)))))

(defun toss-coin ()
  ;; random function takes n as an argument and returns a random number from 0 to n-1
  (let ((user-call (ask-to-call))
        (result (random 2)))
    (cond 
      ((= user-call result)
       (format t "You won the call. "))
      (t
       (format t "You lost the call. ")))
    
    (cond
      ((= result 0)
       (format t "It was head.~%"))
      (t 
       (format t "It was tails.~%")))))

(toss-coin)
