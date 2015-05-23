#lang racket

(require redex)

(provide accelerate subst evaluation-rules generate-indices get
         scalar-eval lookup scalar-generate expand-shape filter-shape
         valid-array permute-fold array-update shapeSize flatten-index
         iota listRef prependAll flatten fold-every filter-indices
         match-shape-filter?)

(define-language accelerate
  ((array-exp ae) (use arrconst) v (map (λ (x) e) ae)
                  (generate shape (λ (x ...) e)) (let (x ae) ae)
                  a (zipWith (λ (x y) e) ae ae)
                  (fold (λ (x y) e) e ae)
                  (permute (λ (x y) e) ae (λ (x ...) e) ae)
                  (backpermute shape (λ (x ...) e) ae)
                  (reshape shape ae) (slice shape ae)
                  (replicate shape ae))
  (f (λ (x ...) e))
  ((scalar-exp e) (let (x e) e) x c (prim e ...) (idx e ...) (? e e e))
  (prim + - * / = <)
  ((x x* a y z) variable-not-otherwise-mentioned)
  ((dim-or-hole s) e box) ;; Use box instead of hole because hole means something special to Redex.
  ((full-shape shape σ) (s ...))
  (i number)
  ((const c c*) i #f #t (idx i ...))
  ((arrconst ac) (array shape c ...))
  ((value v) arrconst)
  
  (C hole (map (λ (x) e) C) (let (x C) ae) (zipWith (λ (x y) e) C ae) ;(zipWith (λ (x y) e) ae C)
     (zipWith (λ (x y) e) arrconst C) (fold (λ (x y) e) e C)
     (permute f C f ae) (permute f arrconst f C) ;(permute f ae f C)
     (reshape shape C) (slice shape C) (replicate shape C)
     (backpermute shape (λ (x ...) e) C))
  
  (env ((x c) ...)))

(define-metafunction accelerate
  valid-array : arrconst -> #t or #f
  [(valid-array (array σ c ...))
   ,(= (term (shapeSize σ)) (length (term (c ...))))])

(define-metafunction accelerate
  apply-prim : prim c c -> c
  [(apply-prim + c_1 c_2)
   ,(+ (term c_1) (term c_2))]
  [(apply-prim - c_1 c_2)
   ,(- (term c_1) (term c_2))]
  [(apply-prim * c_1 c_2)
   ,(* (term c_1) (term c_2))]
  [(apply-prim / c_1 c_2)
   ,(/ (term c_1) (term c_2))]
  [(apply-prim = c_1 c_2)
   ,(= (term c_1) (term c_2))]
  [(apply-prim < c_1 c_2)
   ,(< (term c_1) (term c_2))])

(define-metafunction accelerate
  [(scalar-eval c env) c]
  [(scalar-eval x env) (lookup x env)]
  [(scalar-eval (prim e_1 e_2) env)
   (apply-prim prim
               (scalar-eval e_1 env)
               (scalar-eval e_2 env))]
  [(scalar-eval (idx e ...) env)
   (idx (scalar-eval e env) ...)]
  [(scalar-eval (let (x e_0) e) ((y c) ...))
   (scalar-eval e ((x (scalar-eval e_0 ((y c) ...))) (y c) ...))]
  [(scalar-eval (? e_0 e_1 e_2) env)
   (scalar-eval e_2 env)
   (side-condition (eq? (term (scalar-eval e_0 env)) (term #f)))]
  [(scalar-eval (? e_0 e_1 e_2) env)
   (scalar-eval e_1 env)])

(define-metafunction accelerate
  [(lookup x ((x c) (y c*) ...))
   c]
  [(lookup x ((y_!_x c) (z c*) ...))
   (lookup x ((z c*) ...))])

(define-metafunction accelerate
  [(subst x v x) v]
  [(subst x v y) y]
  [(subst x v_1 v_2) v_2]
  [(subst x v (map (λ (y) e) ae))
   (map (λ (y) e) (subst x v ae))]
  [(subst x v (fold (λ (y z) e) e_0 ae))
   (fold (λ (y z) e) e_0 (subst x v ae))]
  [(subst x v (zipWith (λ (y z) e) ae_1 ae_2))
   (zipWith (λ (y z) e)
            (subst x v ae_1)
            (subst x v ae_2))]
  [(subst x v (slice σ ae))
   (slice σ (subst x v ae))]
  [(subst x v (let (x ae_1) ae_2))
   (let (x (subst x v ae_1))
     ae_2)]
  [(subst x v (let (y ae_1) ae_2))
   (let (y (subst x v ae_1))
     (subst x v ae_2))]
  [(subst x v (generate σ (λ (y ...) e)))
   (generate σ (λ (y ...) e))]
  [(subst x v (replicate σ ae))
   (replicate σ (subst x v ae))])

;(define (iota n)
;  (let loop ((n (sub1 n)) (acc '()))
;    (if (zero? n)
;        (cons 0 acc)
;        (loop (sub1 n) (cons n acc)))))

(define-metafunction accelerate
  replicateList : (any ...) i -> (any ...)
  [(replicateList any 0) ()]
  [(replicateList (any ...) i)
   (any ... any_1 ...)
   (where (any_1 ...) (replicateList (any ...) ,(- (term i) (term 1))))])

(define-metafunction accelerate
  [(prependAll any_0 ((any_1 ...) ...))
   ((any_0 any_1 ...) ...)])

(define-metafunction accelerate
  [(flatten ((any_0 ...) ...))
   (any_0 ... ...)])
 
(define-metafunction accelerate
  generate-indices : shape -> ((i ...) ...)
  [(generate-indices (s)) ((i) ...)
   (where (i ...) (iota s))]
  [(generate-indices (s_0 s ...))
   (flatten ((prependAll i_0 any) ...))
   (where (i_0 ...) (iota s_0))
   (where any (generate-indices (s ...)))])

(define-metafunction accelerate
  [(iota i)
   (iota i 0)]
  [(iota 0 i) ()]
  [(iota i_0 i_1)
   (i_1 i_2 ...) (where (i_2 ...) (iota ,(- (term i_0) (term 1)) ,(+ (term i_1) (term 1))))])

(define-metafunction accelerate
  [(scalar-generate (s ...) (x ...) e)
   (array (s ...) (scalar-eval e ((x i) ...)) ...)
   (where ((i ...) ...) (generate-indices (s ...)))])

(define-metafunction accelerate
  [(flatten-index (i ...) (s ...))
   (flatten-index (i ...) (s ...) 0)]
  [(flatten-index () () i) i]
  [(flatten-index (i_0 i ...) (s_0 s ...) i_j)
   (flatten-index (i ...) (s ...)
                  ,(+ (* (term i_j) (term s_0)) (term i_0)))])

;(define (flatten-index i s)
;  (let loop ((i i)
;             (s s)
;             (j 0))
;    (if (null? i)
;        j
;        (loop (cdr i) (cdr s) (+ (* j (car s)) (car i))))))

(define-metafunction accelerate
  listRef : (any ...) i -> any
  [(listRef (any_0 any ...) 0) any_0]
  [(listRef (any_0 any ...) i) (listRef (any ...) ,(- (term i) (term 1)))])

(define-metafunction accelerate
  [(get (idx s ...) arrconst) (get (s ...) arrconst)]
  [(get (s ...) (array (s_2 ...) c ...))
   (listRef (c ...) (flatten-index (s ...) (s_2 ...)))])

;(define (fold-every len x y body init c*)
;  (let loop ((i len)
;             (acc init)
;             (c* c*))
;    (cond
;      [(zero? i) (cons acc (loop len init c*))]
;      [(null? c*) '()]
;      [else (loop (- i 1) (term (scalar-eval ,body ((,x ,acc) (,y ,(car c*))))) (cdr c*))])))

(define-metafunction accelerate
  [(fold-every i_0 x y e c_0 (c ...))
   (fold-every i_0 x y e c_0 (c ...) i_0 c_0)]
  [(fold-every i_0 x y e c_0 (c ...) 0 c_1)
   (c_1 c_2 ...)
   (where (c_2 ...) (fold-every i_0 x y e c_0 (c ...) i_0 c_0))]
  [(fold-every i_0 x y e c_0 () i_1 c_1) ()]
  [(fold-every i_0 x y e c_0 (c c_2 ...) i_1 c_1)
   (fold-every i_0 x y e c_0 (c_2 ...) ,(- (term i_1) (term 1))
               (scalar-eval e ((x c_1) (y c))))])

(define evaluation-rules
  (reduction-relation
   accelerate
   
   ;; Use
   (--> (in-hole C (use arrconst))
        (in-hole C arrconst)
        E-use
        (side-condition (term (valid-array arrconst))))
   ;; Generate
   (--> (in-hole C (generate (s ...) (λ (x ...) e)))
        (in-hole C (scalar-generate (s ...) (x ...) e))
        E-generate
        (side-condition (= (length (term (s ...))) (length (term (x ...))))))
   ;; Map
   (--> (in-hole C (map (λ (x) e) (array shape c ...)))
        (in-hole C (array shape
                          (scalar-eval e ((x c))) ...))
        E-map)
   ;; Let
   (--> (in-hole C (let (x v) ae))
        (in-hole C (subst x v ae))
        E-let)
   ;; zipWith
   (--> (in-hole C (zipWith (λ (x y) e) (array shape c_1 ...) (array shape c_2 ...)))
        (in-hole C (array shape
                          (scalar-eval e ((x c_1) (y c_2))) ...))
        E-zipWith)
   ;; fold
   (--> (in-hole C (fold (λ (x y) e) e_0 (array (s ... s_0) c ...)))
        (in-hole C (array (s ...)
                          c_1 ...))
        E-fold
        (where (c_1 ...) (fold-every s_0 x y e (scalar-eval e_0 ()) (c ...))))
   
   ;; permute
   (--> (in-hole C (permute (λ (x y) e_0) (array shape c_0 ...) (λ (z ...) e_1) (array shape c ...)))
        ;; This is a very complicated operator. The first lambda is a
        ;; fold function, which is used to combine different values
        ;; that land on the same location. The first array is a set of
        ;; initial values. The second function maps indices, and the
        ;; last function provides the data to map to each index.
        (in-hole C (permute-fold (λ (x y) e_0) (array shape c_0 ...) (c_1 ...) (c ...)))
        E-permute
        ;; First, generate the indices, since we need to compute where they all go.
        (where (array shape c_1 ...) (scalar-generate shape (z ...) e_1)))
   
   ;; backpermute
   (--> (in-hole C (backpermute (s ...) (λ (x ...) e) arrconst))
        (in-hole C (array (s ...) (get (scalar-eval e ((x i) ...)) arrconst) ...))
        E-backpermute
        (where ((i ...) ...) (generate-indices (s ...)))
        (side-condition (= (length (term (s ...))) (length (term (x ...))))))
   ;; reshape
   (--> (in-hole C (reshape (s ...) (array shape c ...)))
        (in-hole C (array (s ...) c ...))
        E-reshape
        (side-condition (= (length (term (c ...))) (term (shapeSize (s ...))))))
   
   ;; slice
   (--> (in-hole C (slice (s_1 ...) (array (s_2 ...) c ...)))
        (in-hole C (array (filter-shape (s_1 ...) (s_2 ...))
                          (get (i ...) (array (s_2 ...) c ...)) ...))
        E-slice
        (where ((i ...) ...) (filter-indices (s_1 ...) (generate-indices (s_2 ...))))
        (side-condition (= (length (term (s_1 ...))) (length (term (s_2 ...))))))
   
   ;; replicate
   (--> (in-hole C (replicate (s_1 ...) (array (s_2 ...) c ...)))
        (in-hole C (array (expand-shape (s_1 ...) (s_2 ...))
                          (get (filter-shape shape_1 (i ...))
                               (array (s_2 ...) c ...)) ...))
        E-replicate
        (where shape_1 (s_1 ...))
        (where ((i ...) ...) (generate-indices (expand-shape (s_1 ...) (s_2 ...)))))))

(define-metafunction accelerate
  permute-fold : f arrconst (c ...) (c ...) -> arrconst
  [(permute-fold f arrconst () ())
   arrconst]
  [(permute-fold f arrconst ((idx i ...) c_0 ...) (c c_1 ...))
   (permute-fold f (array-update f arrconst (i ...) c) (c_0 ...) (c_1 ...))]
  [(permute-fold f arrconst (#f c_0 ...) (c c_1 ...))
   (permute-fold f arrconst (c_0 ...) (c_1 ...))])

(define-metafunction accelerate
  array-update : f arrconst (i ...) c -> arrconst
  [(array-update (λ (x y) e) (array σ c_0 ... c_1 c_2 ...) (i ...) c)
   (array σ c_0 ... (scalar-eval e ((x c_1) (y c))) c_2 ...)
   (side-condition (= (length (term (c_0 ...)))
                      (term (flatten-index (i ...) σ))))])

(define-metafunction accelerate
  [(expand-shape (box s_1 ...) (s s_2 ...))
   (s s_3 ...) (where (s_3 ...) (expand-shape (s_1 ...) (s_2 ...)))]
  [(expand-shape (s_1 s_2 ...) (s_3 ...))
   (s_1 s_4 ...) (where (s_4 ...) (expand-shape (s_2 ...) (s_3 ...)))]
  [(expand-shape shape ()) shape])

(define-metafunction accelerate
  [(filter-shape () ()) ()]
  [(filter-shape (box s_1 ...) (s s_2 ...))
   (s s_3 ...) (where (s_3 ...) (filter-shape (s_1 ...) (s_2 ...)))]
  [(filter-shape (s_1 s_2 ...) (s_3 s_4 ...))
   (filter-shape (s_2 ...) (s_4 ...))])

;(define (match-shape-filter? filter idx)
;  (cond
;    [(and (null? filter) (null? idx))
;     #t]
;    [(and (pair? filter) (pair? idx))
;     (and (or (eq? (car filter) 'box)
;              (eq? (car filter) (car idx)))
;          (match-shape-filter? (cdr filter) (cdr idx)))]
;    [else #f]))

(define-metafunction accelerate
  [(match-shape-filter? () ()) #t]
  [(match-shape-filter? (box s_0 ...) (s s_1 ...))
   (match-shape-filter? (s_0 ...) (s_1 ...))]
  [(match-shape-filter? (s s_0 ...) (s s_1 ...))
   (match-shape-filter? (s_0 ...) (s_1 ...))]
  [(match-shape-filter? (s_0 ...) (s_1 ...))
   #f])

(define-metafunction accelerate
  [(filter-indices shape ()) ()]
  [(filter-indices shape ((i_0 ...) (i ...) ...))
   ((i_0 ...) (i_1 ...) ...)
   (side-condition (term (match-shape-filter? shape (i_0 ...))))
   (where ((i_1 ...) ...) (filter-indices shape ((i ...) ...)))]
  [(filter-indices shape ((i_0 ...) (i ...) ...))
   (filter-indices shape ((i ...) ...))])

(define-metafunction accelerate
  shapeSize : shape -> number
  [(shapeSize ()) 1]
  [(shapeSize (s_0 s ...))
   ,(* (term s_0) (term (shapeSize (s ...))))])

;(traces evaluation-rules (term (permute (λ (a b) (+ a b))   (array (2 2) 1 1 1 1)
;                                        (λ (x y) (idx y x)) (array (2 2) 0 1 2 3))))

;(traces evaluation-rules (term (let (a (array () 5))
;                                 (let (a (array () 4))
;                                   a))))

;(traces evaluation-rules (term (let (a (array () 5))
;                                 (let (b (array () 4))
;                                   a))))

;(traces evaluation-rules (term (let (a (array () 1))
;                                 (let (b (array () 2))
;                                   (let (c (array () 3))
;                                     b)))))