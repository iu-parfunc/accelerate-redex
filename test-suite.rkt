#lang racket

(require redex)
(require "accelerate-core.rkt")

(provide test-suite)

;; Some examples, test cases, etc.
(define (test-suite evaluation-rules)
  (test-->> evaluation-rules
            (term (let (a (array () 1))
                    (let (b (array () 2))
                      (let (c (array () 3))
                        b))))
            (term (array () 2)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (let (a (array () 1))
                    (let (a (array () 2))
                      (let (a (array () 3))
                        a))))
            (term (array () 3)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (permute (λ (a b) (+ a b))   (array (2 2) 1 1 1 1)
                           (λ (x y) (idx y x)) (array (2 2) 0 1 2 3)))
            (term (array (2 2) 1 3 2 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (use (array (5) 0 1 2 3 4)))
            (term (array (5) 0 1 2 3 4)))
  (display ".") (flush-output)
  ;; TODO: We really want to say this term doesn't reduce to a value
  (test-->> evaluation-rules
            (term (use (array (2) 0 1 2)))
            (term (use (array (2) 0 1 2))))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (use (array (2 2) 0 1 2 3)))
            (term (array (2 2) 0 1 2 3)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (map (λ (x) 5) (use (array (2 2) 0 1 2 3))))
            (term (array (2 2) 5 5 5 5)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (map (λ (x) x) (use (array (2 2) 0 1 2 3))))
            (term (array (2 2) 0 1 2 3)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (let (x (map (λ (x) 5) (use (array (2) 4 5)))) x))
            (term (array (2) 5 5)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (let (x (array (1) 1)) x))
            (term (array (1) 1)))
  (test-->> evaluation-rules
            (term (reshape (2 2) (array (4) 1 2 3 4)))
            (term (array (2 2) 1 2 3 4)))
  (display ".") (flush-output)
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (slice (1) (array (4) 1 2 3 4)))
            (term (array () 2)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (slice (0 box) (array (2 2) 1 2 3 4)))
            (term (array (2) 1 2)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (slice (box 0) (array (2 2) 1 2 3 4)))
            (term (array (2) 1 3)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (fold (λ (x y) (+ x y)) 0 (array (4) 1 1 1 1)))
            (term (array () 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (replicate (4) (array () 1)))
            (term (array (4) 1 1 1 1)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (generate (5) (λ (x) x)))
            (term (array (5) 0 1 2 3 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (generate (3 3) (λ (x y) (+ x y))))
            (term (array (3 3) 0 1 2 1 2 3 2 3 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (generate (2 2 2) (λ (x y z) (* x (* y z)))))
            (term (array (2 2 2) 0 0 0 0 0 0 0 1)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (let (t (array (1) 5))
                    (map (λ (x) (+ 1 x)) t)))
            (term (array (1) 6)))
  (display ".") (flush-output)
  ;; TODO: We really want to say this term doesn't reduce to a value
  (test-->> evaluation-rules
            (term (reshape (3) (array (4) 1 2 3 4)))
            (term (reshape (3) (array (4) 1 2 3 4))))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (replicate (2 box box) (array (2 3)
                                                1 2 3
                                                4 5 6)))
            (term (array (2 2 3)
                         1 2 3
                         4 5 6
                         1 2 3
                         4 5 6)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (slice (0 box 1 box 2)
                         (replicate (4 box 6 box 4) (array (2 2) 1 2 3 4))))
            (term (array (2 2) 1 2 3 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (backpermute (2 2) (λ (i j) (idx j i)) (array (2 2) 1 2 3 4)))
            (term (array (2 2) 1 3 2 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (fold (λ (a b) (* a b)) 1 (array (3 4)
                                                   1 1 1 1
                                                   2 2 2 2
                                                   2 3 4 5)))
            (term (array (3) 1 16 120)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (let (a (array (2 2) 1 1 1 1))
                    (fold (λ (x y) x) 4 a)))
            (term (array (2) 4 4)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (generate (4 4) (λ (i j) (? (= i j) 1 0))))
            (term (array (4 4)
                         1 0 0 0
                         0 1 0 0
                         0 0 1 0
                         0 0 0 1)))
  (display ".") (flush-output)
  (test-->> evaluation-rules
            (term (zipWith (λ (a b) (* a b))
                           (generate (2 2) (λ (x y) 2))
                           (generate (2 2) (λ (x y) (+ x y)))))
            (term (array (2 2) 0 2 2 4)))
  (display ".")
  (newline)
  (test-results))
