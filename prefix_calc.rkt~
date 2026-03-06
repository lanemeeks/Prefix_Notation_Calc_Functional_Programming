#lang racket
;tail-recursive helper function that given the prefix expression
;as a string it returns the result as a number
(define (eval-exp expression-str history)
  (define expression (with-input-from-string expression-str read));parsing string like input makes into data


  ;defining another helper method to interpret use of $n
  (define (int-hist s)
    (cond
      ((symbol? s)
       (let* ((str (symbol->string s)))
         (if (and (> (string-length str) 1)
                  (char=? (string-ref str 0) #\$));establishing if the preceeding symbol was $
             (let* ((id (string->number (substring str 1)))
                    (rev (reverse history)))
               (if (and id (<= 1 id) (<= id (length rev)))
                   (list-ref rev (- id 1))
                   (error "Error: Invalid history reference")))
             s)))
      (else s)))
  
  ;helper function to remove nested lists within expression
  (define (remove-nests l)
    (cond
      ((number? l) (list l));returning the number as a list
      ((symbol? l) (list (int-hist l)));dealing with history reference case
      ((list? l) (cons (car l)
                       (apply append (map remove-nests (cdr l)))));recursively remove lists inside the main one
      (else (error "Error: Input contains invalid content"))))
  (define tokens (reverse (remove-nests expression)));remove nested statements and put them back without parenthesis and then reverse it

  (define (apply-operator op a b);cute lil method to avoid using eval
    (cond
      ((eq? op '+) (+ a b))
      ((eq? op '-) (- a b))
      ((eq? op '*) (* a b))
      ((eq? op '/)
       (if (zero? b)
           (error "Error: Division by zero")
           (/ a b)))
      (else (error "Error: Bad operator"))))



  (let eval-loop ((remaining tokens);remaining is the tokens left to process
                  (stack '()));defining empty list to act as a 'stack'
    (cond
      ((null? remaining)
       (if (= (length stack) 1)
              (car stack);'popping' when there's one number left
              (error "Error: Input was invalid format"))); if there nothing in the stack something went wrong
       ((number? (car remaining));if it's a number
        (eval-loop (cdr remaining)
                   (cons (car remaining) stack)));call eval-loop again
       (else;token is operator case
        (if (< (length stack) 2)
            (error "Error: too few operands")
            (let* ((op (car remaining))
               (a (car stack))
               (b (cadr stack));get things from the stack
               (rest (cddr stack))
               (result (apply-operator op a b)))
          (eval-loop (cdr remaining)
                     (cons result rest))))))));tail-recursion call it again on the rest of the things

(define (user-input)
  (let input-loop ((history '()))
    (display "Enter prefix expression or \"quit\" to exit: ")
    (flush-output);makes sure the prompt is displayed

    (define input (read-line))
    
    (cond
      ((or (eof-object? input)
           (string=? input "quit"));user wants to quit the program
       (displayln "Ending Program"))
      ((or (string=? input "")
          (regexp-match? #px"^\\s*$" input));checking if input is empty or just white spaces
       (error "Error: Input is empty")
       (input-loop history))
      (else
       (with-handlers
           ((exn:fail?;try-catch statement checking for run time errors
             (lambda (e)
               (error "Error: runtime or syntax error caught by program")
               (input-loop history))))
         (let* ((result (eval-exp input history))
                (new-history (cons result history));adding the result to the history list
                (id (length new-history))
                (float-val (real->double-flonum result)))
           (display id)
           (display ": ")
           (display float-val)
           (newline)
           (input-loop new-history)))))))
