#lang info

;; pkg info

(define collection "scribble-pict")
(define deps '("base" "scribble-pict-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define pkg-authors '(ryanc))

;; collection info

(define name "scribble-pict")
#;(define scribblings '(("scribblings/scribble-pict.scrbl" ())))

(define compile-omit-paths '("examples" "misc"))
(define test-omit-paths '("examples" "misc"))
