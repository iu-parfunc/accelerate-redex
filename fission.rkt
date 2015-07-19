#lang racket

(require redex)
(require "accelerate-core.rkt")

(provide evaluation-rules+fission accelerate+fission
         split-left split-right shape-of rank newIndex realDim left-shape
         right-shape eval-concat add-index sub-index)

(define-extended-language accelerate+fission accelerate
  (ae ....
   (tuple ae ae)
   (fst ae)
   (snd ae)
   (split c ae)
   (concat c ae ae))
  
  (v ....
     (tuple v v))

  (C ....
   (tuple C ae)
   ;(tuple ae C)
   (tuple v C)
   (fst C)
   (snd C)
   (split c C)
   (concat c C ae)
   ;(concat c ae C))
   (concat c arrconst C))
  
  ((fission-point F) hole
                     (map f F)
                     (let (x F) ae)  (let (x ae) F)
                     (zipWith f F ae)  (zipWith f ae F) 
                     (fold f e F)
                     (backpermute σ f F) 
                     (permute f F f ae)  (permute f ae f F)
                     (reshape σ F)
                     (slice σ F)
                     (replicate σ F)
                     (concat i F ae)  (concat i ae F)
                     (tuple F ae)  (tuple ae F)
                     (split i F)))

(define-metafunction accelerate+fission
  [(realDim shape i)
   ,(+ (length (term shape)) (term i))
   (side-condition (< (term i) (term 0)))]
  [(realDim shape i) i])

(define-metafunction accelerate+fission
  [(left-shape 0 (s_0 s ...))
   (,(floor (/ (term s_0) (term 2))) s ...)]
  [(left-shape i (s_0 s ...))
   (s_0 s_1 ...)
   (where (s_1 ...) (left-shape ,(- (term i) (term 1)) (s ...)))])

(define-metafunction accelerate+fission
  [(right-shape 0 (s_0 s ...))
   (,(ceiling (/ (term s_0) (term 2))) s ...)]
  [(right-shape i (s_0 s ...))
   (s_0 s_1 ...)
   (where (s_1 ...) (right-shape ,(- (term i) (term 1)) (s ...)))])

(define-metafunction accelerate+fission
  [(split-left i_0 (array (s ...) c ...))
   (array (s_1 ...) (get (i_1 ...) (array (s ...) c ...)) ...)
   (where i (realDim (s ...) i_0))
   (where (s_1 ...) (left-shape i (s ...)))
   (where ((i_1 ...) ...) (generate-indices (s_1 ...)))])

(define (shift-idx idx i shift)
  (let loop ((i i)
             (s idx))
    (if (zero? i)
        (cons (+ (car s) shift)
              (cdr s))
        (cons (car s) (loop (- i 1) (cdr s))))))

(define-metafunction accelerate+fission
  [(add-index 0 (s_0 s_1 ...) (s_2 s_3 ...))
   (,(+ (term s_0) (term s_2)) s_3 ...)]
  [(add-index i (s_0 s_1 ...) (s_2 s_3 ...))
   (s_2 s_4 ...)
   (where (s_4 ...) (add-index ,(- (term i) (term 1)) (s_1 ...) (s_3 ...)))])

(define-metafunction accelerate+fission
  [(sub-index 0 (s_0 s_1 ...) (s_2 s_3 ...))
   (,(- (term s_2) (term s_0)) s_3 ...)]
  [(sub-index i (s_0 s_1 ...) (s_2 s_3 ...))
   (s_2 s_4 ...)
   (where (s_4 ...) (sub-index ,(- (term i) (term 1)) (s_1 ...) (s_3 ...)))])

(define-metafunction accelerate+fission
  [(split-right i_0 (array (s ...) c ...))
   (array shape_1 (get (add-index i shape_0 (i_1 ...))
                        (array (s ...) c ...)) ...)
   
   (where i (realDim (s ...) i_0))
   (where shape_0 (left-shape i (s ...)))
   (where shape_1 (right-shape i (s ...)))
   (where ((i_1 ...) ...) (generate-indices shape_1))])

(define-metafunction accelerate+fission
  [(get-two i_0 (i_1 ... i i_2 ...)
            (array (s_0 ... s s_1 ...) c_0 ...)
            (array (s_2 ...) c_1 ...))
   (get (i_1 ... i i_2 ...) (array (s_0 ... s s_1 ...) c_0 ...))
   (side-condition (= (term i_0) (length (term (s_0 ...)))))
   (side-condition (= (term i_0) (length (term (i_1 ...)))))
   (side-condition (< (term i) (term s)))]
  [(get-two i_0 (i ...)
            (array (s_0 ... s s_1 ...) c_0 ...)
            (array (s_2 ...) c_1 ...))
   (get (sub-index i_0 (s_0 ... s s_1 ...) (i ...)) (array (s_2 ...) c_1 ...))
   (side-condition (= (term i_0) (length (term (s_0 ...)))))])
   

(define-metafunction accelerate+fission
  [(eval-concat i
                (array (s_1 ...) c_1 ...)
                (array (s_2 ...) c_2 ...))
   ;; TODO: check that s_1 and s_2 are equal at all points except the dimension we're concatenating.
   (array shape_1
          (get-two i (i_1 ...)
                   (array (s_1 ...) c_1 ...)
                   (array (s_2 ...) c_2 ...)) ...)
   (where shape_1 (add-index i (s_1 ...) (s_2 ...)))
   (where ((i_1 ...) ...) (generate-indices (add-index i (s_1 ...) (s_2 ...))))])

(define-metafunction/extension subst accelerate+fission
  [(subst+fission x v (tuple ae_1 ae_2))
   (tuple (subst+fission x v ae_1)
          (subst+fission x v ae_2))]
  [(subst+fission x v (concat i ae_1 ae_2))
   (concat i
           (subst+fission x v ae_1)
           (subst+fission x v ae_2))]
  [(subst+fission x v (split i ae))
   (split i (subst+fission x v ae))]
  [(subst+fission x v (fst ae))
   (fst (subst+fission x v ae))]
  [(subst+fission x v (snd ae))
   (snd (subst+fission x v ae))])

(define evaluation-rules+fission
  (extend-reduction-relation
   evaluation-rules accelerate+fission
   ;; Do we need to evaluate tuples all the way to a value before doing fst and snd?
   (--> (in-hole C (fst (tuple v_1 v_2)))
        (in-hole C v_1)
        E-fst)
   (--> (in-hole C (snd (tuple v_1 v_2)))
        (in-hole C v_2)
        E-snd)
   (--> (in-hole C (split i arrconst))
        (in-hole C (tuple (split-left i arrconst)
                          (split-right i arrconst)))
        E-split)
   (--> (in-hole C (concat i arrconst_1 arrconst_2))
        (in-hole C (eval-concat i arrconst_1 arrconst_2))
        E-concat
        (side-condition (< (term i) (term (rank arrconst_1))))
        (side-condition (< (term i) (term (rank arrconst_2)))))

   ;; Let
   ;; I'm not really sure what the point of metafunction extension is if you still have to do this.
   (--> (in-hole C (let (x v) ae))
        (in-hole C (subst+fission x v ae))
        E-let)

   ;; These are the fission rules
   
   ;; fold-inner
   ;; TODO: need a termination condition
   (--> (in-hole F (fold (λ (x y) e) e_0 ae))
        (in-hole F (let (t (split -1 ae))
                     (zipWith (λ (x y) e)
                              (fold (λ (x y) e) e_0 (fst t))
                              (fold (λ (x y) e) e_0 (snd t)))))
        F-fold-inner
        (where (s ... s_n) (shape-of ae))
        (side-condition (> (term s_n) (term 1))))
   ;; fold-outer
   (--> (in-hole F (fold (λ (x y) e) e_0 ae))
        (in-hole F (let (t (split 0 ae))
                     (concat 0
                             (fold (λ (x y) e) e_0 (fst t))
                             (fold (λ (x y) e) e_0 (snd t)))))
        F-fold-outer
        (where (s_0 s ...) (shape-of ae))
        (side-condition (> (term s_0) (term 1)))
        (side-condition (> (term (rank ae)) (term 1))))
   
   ;; map
   (--> (in-hole F (map (λ (x) e) ae))
        (in-hole F (let (t (split ,(length (term (s_1 ...))) ae))
                     (concat ,(length (term (s_1 ...)))
                             (map (λ (x) e) (fst t))
                             (map (λ (x) e) (snd t)))))
        F-map
        (where (s_1 ... s s_2 ...) (shape-of ae))
        (side-condition (> (term s) (term 1))))

   ;; generate
   (--> (in-hole F (generate (s_1 ... s s_2 ...) (λ (x_1 ... x x_2 ...) e)))
        (in-hole F (concat i 
                           (generate (s_1 ... ,(floor (/ (term s) (term 2))) s_2 ...)
                                     (λ (x_1 ... x x_2 ...) e))
                           (generate (s_1 ... ,(ceiling (/ (term s) (term 2))) s_2 ...)
                                     (λ (x_1 ... x x_2 ...)
                                       (let (x (+ x ,(floor (/ (term s) (term 2)))))
                                         e)))))
        F-generate
        (where i ,(length (term (s_1 ...))))
        (side-condition (= (length (term (s_1 ...))) (length (term (x_1 ...)))))
        (side-condition (> (term s) (term 1))))
   
   ;; reshape
   ;; - the rule in the paper looks suspect. I vote for leaving it out.
   
   ;; slice - this rule is also currently unsound.
   ;(--> (in-hole F (slice σ ae))
   ;     (in-hole F (let (t (split i ae))
   ;                  (concat i (slice σ (fst t)) (slice σ (snd t)))))
   ;     F-slice
   ;     (where (s_0 ... s_i s_1 ...) (shape-of ae))
   ;     (where i ,(length (term (s_0 ...)))))
   
   ;; replicate
   (--> (in-hole F (replicate σ ae))
        (in-hole F (let (t (split i ae))
                     (concat (newIndex σ i)
                             (replicate σ (fst t))
                             (replicate σ (snd t)))))
        F-replicate
        (where (s_0 ... s_i s_1 ...) (shape-of ae))
        (where i ,(length (term (s_0 ...))))
        (side-condition (> (term s_i) (term 1))))
   
   ;; zipWith
   (--> (in-hole F (zipWith (λ (x y) e) ae_1 ae_2))
        (in-hole F (let (a (split i ae_1))
                     (let (b (split i ae_2))
                       (concat i (zipWith (λ (x y) e) (fst a) (fst b))
                                 (zipWith (λ (x y) e) (snd a) (snd b))))))
        F-zipWith
        (where (s_0 ... s_i s_1 ...) (shape-of ae_1))
        (where i ,(length (term (s_0 ...))))
        (fresh a b)
        (side-condition (> (term s_i) (term 1))))
   
   ;; backpermute
   (--> (in-hole F (backpermute (s_0 ... s s_1 ...) (λ (x_0 ... x x_1 ...) e) ae))
        (in-hole F (concat i (backpermute (s_0 ... s_2 s_1 ...) (λ (x_0 ... x x_1 ...) e) ae)
                             (backpermute (s_0 ... s_3 s_1 ...)
                                          (λ (x_0 ... x x_1 ...)
                                            (let (x (+ x s_2)) e))
                                          ae)))
        F-backpermute
        (where s_2 ,(floor (/ (term s) (term 2))))
        (where s_3 ,(ceiling (/ (term s) (term 2))))
        (where i   ,(length (term (s_0 ...))))
        (side-condition (= (length (term (s_0 ...))) (length (term (x_0 ...)))))
        (side-condition (> (term s) (term 1))))
   
   ;; use
   (--> (in-hole F (use (array (s_0 s ...) c_0 ... c_1 ...)))
        (in-hole F (concat 0 (array (,(floor (/ (term s_0) (term 2))) s ...) c_0 ...)
                             (array (,(ceiling (/ (term s_0) (term 2))) s ...) c_1 ...)))
        F-use
        (side-condition (= (length (term (c_0 ...))) (* (floor (/ (term s_0) (term 2))) (term (shapeSize (s ...))))))
        (side-condition (term (valid-array (array (s_0 s ...) c_0 ... c_1 ...))))
        (side-condition (> (term s_0) (term 1))))
   ))

(define-metafunction accelerate+fission
  shape-of : ae -> shape or #f
  [(shape-of (array shape c ...))
   shape]
  [(shape-of (use ae))
   (shape-of ae)]
  [(shape-of (generate σ (λ (x ...) e)))
   σ]
  [(shape-of (let (x ae_0) ae_1)) (shape-of ae_1)]
  [(shape-of (zipWith f ae_1 ae_2)) (shape-of ae_1)]
  [(shape-of (fold (λ (x y) e) e_0 ae))
   (s_0 ...)
   (where (s_0 ... s) (shape-of ae))]
  [(shape-of (backpermute σ f ae)) σ]
  [(shape-of (permute f_1 ae_0 f_2 ae_1)) (shape-of ae_0)]
  [(shape-of (reshape σ ae)) σ]
  [(shape-of (slice σ ae)) (filter-shape σ (shape-of ae))]
  [(shape-of (replicate σ ae)) (expand-shape σ (shape-of ae))]
  [(shape-of (fst (tuple ae_1 ae_2)))
   (shape-of ae_1)]
  [(shape-of (snd (tuple ae_1 ae_2)))
   (shape-of ae_2)]
  [(shape-of (concat i ae_1 ae_2))
   (s_0 ... ,(+ (term s_1) (term s_2)) s_3 ...)
   (where (s_0 ... s_1 s_3 ...) (shape-of ae_1))
   (where (s_0 ... s_2 s_3 ...) (shape-of ae_2))
   (side-condition (= (term i) (length (term (s_0 ...)))))]
  [(shape-of _) #f])

(define-metafunction accelerate+fission
  rank : ae -> number
  [(rank ae) ,(length (term (shape-of ae)))])

(define-metafunction accelerate+fission
  newIndex : shape number -> number
  [(newIndex (box s ...) 0) 0]
  [(newIndex (box s ...) i) ,(+ (term 1) (term (newIndex (s ...) ,(- (term i) (term 1)))))]
  [(newIndex (s_0 s_1 ...) i) ,(+ (term 1) (term (newIndex (s_1 ...) i)))])



;(traces evaluation-rules+fission (term (backpermute (2 2) (λ (i j) (idx j i)) (array (2 2) 1 2 3 4))))

;(traces evaluation-rules+fission (term (use (array (2 3 4)
;                                                  1 2 3 4 5 6 7 8 9 10
;                                                  11 12 13 14 15 16 17 18 19 20
;                                                  21 22 23 24))))

;(traces evaluation-rules+fission (term (replicate (2 box box) (array (2 3)
;                                                                     1 2 3
;                                                                     4 5 6))))

;(stepper evaluation-rules+fission (term (zipWith (λ (x y) (+ x y))
;                                                (array (2 2) 1 2 3 4)
;                                                (array (2 2) 4 3 2 1))))

;(traces evaluation-rules+fission (term (slice (box 0) (array (2 2) 1 2 3 4))))
;(traces evaluation-rules+fission (term (slice (0 box) (array (2 2) 1 2 3 4))))
;(traces evaluation-rules+fission (term (slice (1) (array (4) 1 2 3 4))))

;(traces evaluation-rules+fission (term (map (λ (x) (+ 1 x)) (array (2 2 2) 0 1 2 3 4 5 6 7))))
;(traces evaluation-rules+fission (term (generate (2 2) (λ (x y) (* x y)))))

;; This example makes sure fissioning will eventually happen under lets.
;(traces evaluation-rules+fission (term (let (A (array (2 2) 1 2 3 4))
;                                         (map (λ (x) (+ 1 x)) A))))

;(traces evaluation-rules+fission (term (generate (4) (λ (i) i))))

;(traces evaluation-rules+fission (term (map (λ (x) (+ 1 x))
;                                            (generate (4) (λ (i) i)))))
                                       
;(stepper evaluation-rules+fission
;        (term (zipWith (λ (a b) (* a b))
;                       (generate (2 2) (λ (x y) 2))
;                       (generate (2 2) (λ (x y) (+ x y))))))

;(traces evaluation-rules+fission
;        (term (zipWith (λ (a b) (* a b))
;                       (array (2 2) 2 3 5 7)
;                       (array (2 2) 11 13 17 19))))

;(traces evaluation-rules+fission
;         (term (fold (λ (x y) (+ x y)) 0 (array (4) 1 1 1 1)))) 

;(traces evaluation-rules+fission (term (generate (2 2 2) (λ (x y z) (* x (* y z))))))

;(traces evaluation-rules+fission
;         (term (replicate (2 box box) (array (2 3)
;                                             1 2 3
;                                             4 5 6))))