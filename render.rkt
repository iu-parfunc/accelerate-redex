#lang racket

;; Renders all the figures we might need for the ICFP paper.

(require redex)
(require unstable/gui/redex)
(require file/convertible)
(require racket/serialize)
(require "accelerate-core.rkt")
(require "fission.rkt")

(default-font-size 9)
(label-font-size 9)
(metafunction-font-size 9)

(define (make-pdf pict filename)
  (with-output-to-file filename
    (lambda () (write-bytes (convert pict 'pdf-bytes)))
    #:exists 'replace))

(with-atomic-rewriter 'box "□"
(with-compound-rewriters (('= (binary-rw " = "))
                          ('eq? (binary-rw " = "))
                          ('+ (binary-rw " + "))
                          ('- (binary-rw " - "))
                          ('* (binary-rw " × "))
                          ('/ (binary-rw " ÷ "))
                          ('> (binary-rw " > "))
                          ('< (binary-rw " < "))
                          ('list-ref (function-rw "listRef"))
                          ('if (function-rw "if"))
                          ('length (bracket-rw 'angle))
                          ('floor (bracket-rw '("⎣" "⎦")))
                          ('ceiling (bracket-rw '("⎡" "⎤"))))
  (begin
    (make-pdf (render-language accelerate) "../figures/accelerate-language.pdf")
    
    (rule-pict-style 'horizontal)
    (make-pdf (render-reduction-relation evaluation-rules) "../figures/accelerate-reduction.pdf")
    (rule-pict-style 'horizontal)
    
    (make-pdf (render-language accelerate+fission #:nts (list 'ae 'v 'C))
              "../figures/accelerate-language+fission.pdf")
    (make-pdf (render-language accelerate+fission #:nts '(F))
              "../figures/fission-contexts.pdf")

    (parameterize ((render-reduction-relation-rules '(E-split E-concat E-fst E-snd)))
      (make-pdf (render-reduction-relation evaluation-rules+fission) "../figures/fission-evaluation.pdf"))
    (parameterize ((render-reduction-relation-rules '(F-fold-inner F-fold-outer F-map F-generate
                                                                   F-replicate F-zipWith
                                                                   F-backpermute F-use)))
      (make-pdf (render-reduction-relation evaluation-rules+fission) "../figures/fission-rules.pdf"))

    (metafunction-pict-style 'left-right/beside-side-conditions)
    (make-pdf (render-metafunctions
               scalar-eval
               ;lookup
               scalar-generate
               ;get
               expand-shape
               filter-shape
               permute-fold
               ;array-update
               ;shapeSize
               ;valid-array
               ;flatten-index
               ;iota
               ;listRef
               prependAll
               ;flatten
               generate-indices
               fold-every
               filter-indices
               match-shape-filter?)
              "../figures/accelerate-metafunction.pdf")
    
    (make-pdf (render-metafunctions
               shape-of
               )
              "../figures/fission-shape-of.pdf")
    
    (metafunction-pict-style 'left-right/vertical-side-conditions)
    (make-pdf (render-metafunctions
               split-left
               split-right
               ;eval-concat
               shape-of
               ;rank
               newIndex
               realDim
               ;left-shape
               ;right-shape
               ;add-index
               ;sub-index
               )
              "../figures/fission-metafunction.pdf"))))
