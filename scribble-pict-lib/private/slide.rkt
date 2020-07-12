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
  (void (slides-from-part p no-ctx)))

;; SlideContext is (cons Pict/#f Layout (Listof Pict)) -- title, layout, body picts
(define no-ctx '(#f auto . ()))

;; Slide style names:
;; - 'next : sub-parts successively extend parent
;; - 'alt : sub-parts independently extend parent

(define (slides-from-part p ctx)
  (match p
    [(s:part tag-pfx tags title-content style to-collect blocks parts)
     ;; Note: part styles are not inherited.
     (let ([istyle (add-slide-style style (current-sp-style))])
       (define st (slide-from-part-contents title-content blocks ctx istyle))
       (case (hash-ref istyle 'slide-mode #f)
         [(next)
          (eprintf "NEXT\n~v\n" parts)
          (for/fold ([st st]) ([p (in-list parts)])
            (slides-from-part p st))]
         [(alt)
          (for/fold ([st ctx]) ([p (in-list parts)])
            (slides-from-part p st))]
         [else ;; #f
          (for/fold ([st st]) ([p (in-list parts)])
            (slides-from-part p ctx))]))]))

(define (slide-from-part-contents title-content blocks ctx istyle)
  (match-define (list* ctx-title ctx-layout ctx-prefix) ctx)
  (define title-p
    (match title-content
      [(list "..") ;; !!
       ctx-title]
      [(or #f '()) #f]
      [else
       (let ([istyle (get-title-istyle istyle)])
         (content->pict title-content istyle +inf.0))]))
  (define layout (hash-ref istyle 'slide-layout ctx-layout))
  (define body-p (flow->pict blocks istyle))
  (define full-body (append ctx-prefix (list body-p)))
  (apply p:slide #:title title-p #:layout layout full-body)
  (list* title-p layout full-body))

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
