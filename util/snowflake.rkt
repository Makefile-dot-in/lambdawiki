#lang racket
(require threading
         racket/date)

(provide
 (contract-out
  [new-snowflake (-> exact-integer?)]
  [snowflake->date (-> snowflake? date?)])
 snowflake?)

(define epoch 1765925858)
(define counter 0)
(define snowflake? exact-integer?)

;; generates a new snowflake
(define (new-snowflake)
  (set! counter (bitwise-bit-field (add1 counter) 0 22))
  (bitwise-ior (arithmetic-shift (- (current-seconds) epoch) 22)
               counter))

(define (snowflake->date snowflake)
  (~> snowflake
      (arithmetic-shift -22)
      (+ epoch)
      seconds->date))
