#lang racket

(require redex)

(require "accelerate-core.rkt")
(require "test-suite.rkt")
(require "fission.rkt")

(test-suite evaluation-rules)
(test-suite evaluation-rules+fission)
