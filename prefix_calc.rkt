#lang racket
;tail-recursive helper function that given the prefix expression
;as a string it returns the result as a number
(define (eval-exp expression-str)
  (let eval-loop ((expr (with-input-from-string expression-str read))) ;this is a function that parses the string like input
    (cond
      ((number? expr) expr);checking if its a number
      ((list? expr);handles parenthesis cases
       (let* ((op (car expr));defining the operator
              (args (cdr expr));defining the operands
              (evaluated-args (map eval-loop args)));recursively applying this function to all the operands
         (apply (eval op) evaluated-args)));eval turns operator into function
      (else (error "Basic case error handling, add more later")))))



(define (user-input)
  (let input-loop ()
    (display "Enter prefix expression or \"quit\" to exit: ")
    (flush-output);makes sure the prompt is displayed
    (define input (read-line))
    (if (or (eof-object? input)
            (string=? input "quit"))
        (displayln "Exiting Program")
        (let ((result (eval-exp input)))
          (displayln (string-append "Result: " (number->string result)))
          (input-loop)))))