#lang racket
(define (user-input)
  (let input-loop ()
    (display "Enter prefix expression or \"quit\" to exit: ")
    (flush-output);makes sure the prompt is displayed
    (define input (read-line))
    (if (or (eof-object? input)
            (string=? input "quit"))
        (displayln "Exiting Program")
        (begin
          (displayln input)
          (input-loop)))))