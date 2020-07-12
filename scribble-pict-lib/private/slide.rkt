#lang racket/base
(require racket/match
         racket/list
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         (prefix-in p: slideshow)
         (prefix-in p: pict)
         "pict.rkt")
(provide (all-defined-out))

(define (hash-update* h . kfs)
  (let loop ([h h] [kfs kfs])
    (match kfs
      [(list* k f kfs)
       (loop (hash-update h k f #f) kfs)]
      ['() h])))

;; ----------------------------------------

(define (scribble-slides . pre-parts)
  (define p (s:decode pre-parts))
  (slides-from-part p))

(define (slides-from-part p)
  (match p
    [(s:part tag-pfx tags title-content style to-collect blocks parts)
     ;; Note: part styles are not inherited.
     (let ([istyle (add-slide-style style (current-sp-style))])
       (slide-from-part-contents title-content blocks istyle))
     (for-each slides-from-part parts)]))

(define (slide-from-part-contents title-content blocks istyle)
  (define title-p
    (and title-content
         (let ([istyle (get-title-istyle istyle)])
           (eprintf "title istyle = ~v\n" istyle)
           (content->pict title-content istyle +inf.0))))
  (define body-p (flow->pict blocks istyle))
  (p:slide #:title title-p
           #:layout (hash-ref istyle 'slide-layout 'auto)
           body-p))

(define (add-slide-style s istyle)
  (match s
    [(s:style name props)
     (foldl add-slide-style-prop (add-slide-style name istyle) props)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ istyle]))

(define (add-slide-style-prop prop istyle)
  (match prop
    [(or 'auto 'center 'top 'tall) (hash-set istyle 'slide-layout prop)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ istyle]))

(define (remove-slide-styles istyle)
  (hash-remove* istyle
                '(slide-layout slide-title-color slide-title-size slide-title-base)))

(define (get-title-istyle istyle)
  (remove-slide-styles
   (remove-block-styles
    (hash-update* istyle
                  'color (lambda (v) (hash-ref istyle 'slide-title-color v))
                  'text-size (lambda (v) (hash-ref istyle 'slide-title-size v))
                  'text-base (lambda (v) (hash-ref istyle 'slide-title-base v))))))
