(define listLength
  (lambda (targetList)
    (cond ((empty? targetList) 0)
          (true (+ 1 (listLength (tail targetList)))))))
          
(List.length (list 1 2 3 4 5))