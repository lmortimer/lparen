(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess x) x)))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define sqrt-iter
  (lambda (guess x)
    (cond ((good-enough? guess x) guess)
          ((true) (sqrt-iter (improve guess x) x)))))

(define sqrt
  (lambda (n)
    (sqrt-iter 1.0 x)))