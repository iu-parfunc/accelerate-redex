.phony: all figures

all:
	raco make accelerate.rkt

figures:
	racket render.rkt
