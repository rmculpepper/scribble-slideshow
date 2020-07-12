#lang racket
(require (only-in racket/gui/base))

(define-namespace-anchor a)
(define orig-ns (namespace-anchor->namespace a))

(define modss
  '((slideshow/base
     pict
     pict/code
     ppict/2
     pict/shadow)
    (scribble/base
     scribble/core
     scribble/decode
     scribble/manual
     scribble/example
     scribble/bnf
     scribble/html-properties
     scribble/latex-properties)))

(define (exports mod)
  (define ns (make-base-empty-namespace))
  (parameterize ((current-namespace ns))
    (namespace-attach-module orig-ns 'racket/gui/base)
    (namespace-require mod))
  (namespace-mapped-symbols ns))

(define hs
  (for/list ([mods modss])
    (define h (make-hasheq))
    (for ([mod mods])
      (for ([s (exports mod)])
        (hash-update! h s (lambda (vs) (cons mod vs)) null)))
    h))

(define overlap
  (sort (for/list ([(s ms) (in-hash (car hs))]
                   #:when (for/or ([h (cdr hs)]) (hash-has-key? h s)))
          s)
        symbol<?))

(pretty-write
 (for/list ([s overlap])
   (list s (for/list ([h hs]) (hash-ref h s null)))))

;; ----------------------------------------

#|
((code ((pict/code) (scribble/manual)))
 (item ((slideshow/base) (scribble/manual scribble/base)))
 (para ((slideshow/base) (scribble/manual scribble/base)))
 (t ((slideshow/base) (scribble/manual)))
 (table ((pict) (scribble/core)))
 (tt ((slideshow/base) (scribble/manual scribble/base)))
 (typeset-code ((pict/code) (scribble/manual))))

;; => 

(require (except-in slideshow/base item para t tt)
         (except-in pict table)
         scribble-slideshow)
|#
