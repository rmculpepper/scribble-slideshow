;; Copyright 2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

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

;; FIXME: title picts of different sizes aren't baseline-aligned,
;; looks inconsistent.

(define (hash-update* h . kfs)
  (let loop ([h h] [kfs kfs])
    (match kfs
      [(list* k f kfs)
       (loop (hash-update h k f #f) kfs)]
      ['() h])))

;; ----------------------------------------

(define (scribble-slides . pre-parts)
  (scribble-slides* (s:decode pre-parts)))

(define (scribble-slides* p)
  (define-values (h mk) (slides-from-part p #f))
  (void (mk h no-ctx)))

;; SlideContext is (cons Pict/#f Layout (Listof Pict)) -- title, layout, body picts
(define no-ctx '(#f auto . ()))

;; Slide style names:
;; - 'ignore : do not generate slides
;; - 'next : sub-parts successively extend parent
;; - 'alts : sub-parts independently extend parent

(struct make-slides-prop (mk))

;; slides-from-part : Part Nat/#f -> (values Real (Nat/#f SlideContext -> SlideContext))
;; Returns height of body plus slide-maker function.
(define (slides-from-part p ctx-h)
  (match p
    [(s:part tag-pfx tags title-content style to-collect blocks parts)
     ;; Note: part styles are not inherited.
     (let ([istyle (add-slide-style style (current-sp-style))])
       (define mk0 (or (hash-ref istyle 'slide-maker #f) void))
       (define-values (h mk)
         (case (hash-ref istyle 'slide-mode #f)
           [(ignore)
            (values ctx-h (lambda (h ctx) ctx))]
           [(next)
            (define-values (h1 mk1)
              (slide-from-part-contents title-content blocks ctx-h istyle))
            (for/fold ([h h1] [mks (list mk1)]
                       #:result (values h (do-next (reverse mks))))
                      ([p (in-list parts)])
              (define-values (hp mkp) (slides-from-part p h))
              (values hp (cons mkp mks)))]
           [(alts #f)
            (define-values (h1 mk1)
              (slide-from-part-contents title-content blocks ctx-h istyle))
            (for/fold ([h h1] [mks (list mk1)]
                       #:result (values h (do-alts (reverse mks))))
                      ([p (in-list parts)])
              (define-values (hp mkp) (slides-from-part p ctx-h))
              (values (h-max h hp) (cons mkp mks)))]))
       (cond [ctx-h (values h (lambda (h ctx) (mk0) (mk h ctx)))]
             [else (values #f (lambda (_h ctx) (mk0) (mk h ctx)))]))]))

(define ((do-next mks) h ctx)
  (for/fold ([ctx ctx]) ([mk (in-list mks)])
    (mk h ctx)))

(define ((do-alts mks) h ctx0)
  (for/fold ([ctx ctx0]) ([mk (in-list mks)])
    (mk h ctx0)))

(define (slide-from-part-contents title-content blocks ctx-h istyle)
  (define body-p (flow->pict blocks istyle))
  (define (mk h ctx)
    (match-define (list* ctx-title ctx-layout ctx-prefix) ctx)
    (define title-p
      (match title-content
        [(list "..") ctx-title]
        [(or #f '()) #f]
        [else
         (let ([istyle (get-title-istyle istyle)])
           (content->pict title-content istyle +inf.0))]))
    (define layout (hash-ref istyle 'slide-layout ctx-layout))
    (define full-body (append ctx-prefix (list body-p)))
    (p:slide #:title title-p #:layout layout
             (inset-to-h full-body h))
    (list* title-p layout full-body))
  (values (h+ ctx-h (p:pict-height body-p)) mk))

(define (h-max h1 h2) (if (and h1 h2) (max h1 h2) (or h1 h2)))

(define (h+ h1 h2)
  (cond [h1 (+ h1 h2 (p:current-gap-size))]
        [else h2]))

(define (inset-to-h ps h)
  (define p (apply p:vc-append (p:current-gap-size) ps))
  (if h (values #;p:frame (p:inset p 0 0 0 (- h (p:pict-height p)))) p))

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
    ['next (hash-set istyle 'slide-mode 'next)]
    ['alts (hash-set istyle 'slide-mode 'alts)]
    ['ignore (hash-set istyle 'slide-mode 'ignore)]
    [(make-slides-prop mk) (hash-set istyle 'slide-maker mk)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ istyle]))

(define (remove-slide-styles istyle)
  (define keys '(slide-layout slide-mode
                 slide-title-color slide-title-size slide-title-base))
  (hash-remove* istyle keys))

(define (get-title-istyle istyle)
  (remove-slide-styles
   (remove-block-styles
    (hash-update* istyle
                  'color (lambda (v) (hash-ref istyle 'slide-title-color v))
                  'text-size (lambda (v) (hash-ref istyle 'slide-title-size v))
                  'text-base (lambda (v) (hash-ref istyle 'slide-title-base v))))))

(define (part/make-slides mk)
  (define s (s:style #f (list 'ignore (make-slides-prop mk))))
  (s:part #f null #f s null null null))
