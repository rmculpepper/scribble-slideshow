#lang info

;; pkg info

(define collection "scribble-slideshow")
(define deps '("base" "scribble-slideshow-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define pkg-authors '(ryanc))

;; collection info

(define name "scribble-slideshow")
(define scribblings '(("scribble-slideshow.scrbl" ())))

(define compile-omit-paths '("examples" "misc"))
(define test-omit-paths '("examples" "misc"))
