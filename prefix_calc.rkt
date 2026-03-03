#lang racket
;tail-recursive helper function that given the prefix expression
;as a string it returns the result as a number
(define (eval-exp expression-str)
  (define expression (with-input-from-string expression-str read));parsing string like input makes into data

  ;helper function to remove nested lists within expression
  (define (remove-nests l)
    (cond
      ((number? l) (list l));returning the number as a list
      ((list? l) (cons (car l)
                       (apply append (map remove-nests (cdr l)))));recursively remove lists inside the main one
      (else (error "something is wrong"))))
  (define tokens (reverse (remove-nests expression)));remove nested statements and put them back without parenthesis and then reverse it

  (define (apply-operator op a b);cute lil method to avoid using eval
    (cond
      ((eq? op '+) (+ a b))
      ((eq? op '-) (- a b))
      ((eq? op '*) (* a b))
      ((eq? op '/)
       (if (zero? b)
           (error "Division by zero")
           (/ a b)))))



  (let eval-loop ((remaining tokens);remaining is the tokens left to process
                  (stack '()));defining empty list to act as a 'stack'
    (cond
      ((null? remaining)
       (if (= (length stack) 1)
              (car stack);'popping' when there's one number left
              (error "invalid input"))); if there nothing in the stack something went wrong
       ((number? (car remaining));if it's a number
        (eval-loop (cdr remaining)
                   (cons (car remaining) stack)));call eval-loop again
       (else;token is operator case
        (let* ((op (car remaining))
               (a (car stack))
               (b (cadr stack));get things from the stack
               (rest (cddr stack))
               (result (apply-operator op a b)))
          (eval-loop (cdr remaining)
                     (cons result rest)))))));tail-recursion call it again on the rest of the things

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