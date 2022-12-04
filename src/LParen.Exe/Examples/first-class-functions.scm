(define plus10
  (lambda (x)
    (+ x 10)))

(define operate
  (lambda (fn x)
    (fn x)))
    
(operate plus10 20)

