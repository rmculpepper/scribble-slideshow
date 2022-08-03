;; Copyright 2019-2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/hash
         racket/class
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         pict pict/convert)
(provide (all-defined-out))

(define-logger scribble-slideshow)

(define current-resolve-info (make-parameter #f))

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
(define (hash-remove* h ks) (for/fold ([h h]) ([k (in-list ks)]) (hash-remove h k)))

(define stylist<%>
  (interface ()

    ))

;; ============================================================
;; IStyle (aka SP-Style)

;; An IStyle is an immutable hash mapping style keys (symbols) to values.
;; - There are multiple levels of keys corresponding to different
;;   levels of document structure (eg block vs elem).
;; - Some keys (eg, 'bgcolor) are handled at multiple levels, but most
;;   are specific to one level.
;; - Level-specific keys are generally removed for processing of inner
;;   levels (see `remove-X-styles`); then the result of processing the
;;   inner levels has the outer level's style keys applied (see
;;   `apply-X-styles`).

;; To avoid depending on slideshow, this library duplicates the following
;; default style definitions, mostly following slideshow defaults.

(define BLOCK-WIDTH 800)        ;; (current-para-width) = 738  (!!)
(define WIDE-BLOCK-WIDTH 990)   ;; (current-para-width) = 990 (widescreen)

(define BLOCK-SEP 24)           ;; (current-gap-size) = 24
(define LINE-SEP 5)             ;; (current-line-sep) = 5
(define BASE-SIZE 32)           ;; (current-font-size) = 32
(define TITLE-SIZE 40)          ;; from (current-titlet)
(define TITLE-COLOR "darkred")  ;; (current-title-color) = "black" (!!)
(define TITLE-BASE 'swiss)      ;; (current-main-font) = 'swiss

;; (current-code-font) = (bold . modern)

;; FIXME: Should 'larger, 'smaller, etc change font size (and not
;; affect other picts) or scale?

(define base-istyle
  `#hasheq(;; Block Styles
           (inset-to-width? . #t)
           (block-width     . ,BLOCK-WIDTH)
           (block-sep       . ,BLOCK-SEP)
           (line-sep        . ,LINE-SEP)
           ;; Elem Styles
           (text-base       . default)
           (text-size       . ,BASE-SIZE)
           (scale           . 1)))

(define wide-istyle (hash-set base-istyle 'block-width WIDE-BLOCK-WIDTH))

;; Style properties
(struct text-post-property (post))
(struct elem-post-property (post))
(struct style-transformer (f))

;; Accessors

(define (get-block-width istyle) (hash-ref istyle 'block-width BLOCK-WIDTH))
(define (get-block-sep istyle)   (hash-ref istyle 'block-sep   BLOCK-SEP))
(define (get-line-sep istyle)    (hash-ref istyle 'line-sep    LINE-SEP))

(define (get-code-inset) 0) ;; (/ (get-block-sep) 2)
(define (get-vertical-inset istyle) (get-line-sep istyle))

;; ------------------------------------------------------------

(define (merge-styles base-istyle styleh)
  (for/fold ([istyle base-istyle])
            ([(k v) (in-hash styleh)])
    (cond [(procedure? v) (hash-set istyle k (v base-istyle))]
          [else (hash-set istyle k v)])))

;; ------------------------------------------------------------
;; Basic Styles

;; Elem styles:
;; - 'text-base : (U 'default font% (U 'roman ...) String) -- font face, see `text`
;; - 'text-size : Nat
;; - 'text-mods : (Listof PictTextStyleSymbol) -- see `text`
;; - 'color : (U String color%)
;; - 'bgcolor : (U String color%)
;; - 'white-space : #f | 'pre | 'pre-wrap | 'nowrap
;; - 'text-post : (Listof (Pict -> Pict))
;; - 'elem-post : (Listof (Pict -> Pict))
;; - 'justify : Boolean

(define (add-style s istyle #:ignore-props [ignore-props null])
  (define-values (istyle* props*)
    (add-style* s istyle #:ignore-props ignore-props))
  istyle*)

(define (add-style* s istyle #:ignore-props [ignore-props null])
  (match s
    [(s:style name props)
     (let ([istyle (add-simple-style name istyle)])
       (for/fold ([istyle istyle] [rprops null] #:result (values istyle (reverse rprops)))
                 ([prop (in-list props)])
         (define-values (used? istyle*) (add-style-prop prop istyle ignore-props))
         (values istyle* (if used? rprops (cons prop rprops)))))]
    [s (values (add-simple-style s istyle) null)]))

(define ignore-style-names null)

(define (add-simple-style s istyle)
  (define stylemap (hash-ref istyle 'styles '#hasheq()))
  (cond [(eq? s #f) istyle]
        [(hash-ref stylemap s #f)
         => (lambda (update) (apply-update 'add-style update istyle))]
        [else
         (unless (member s ignore-style-names)
           (log-scribble-slideshow-warning "add-style: ignoring: ~e" s))
         istyle]))

(define (apply-update who update istyle)
  (cond [(list? update)
         (unless (even? (length update))
           (error who "list length is not even: ~e" update))
         (apply hash-set* istyle update)]
        [(hash? update) (merge-styles istyle update)]
        [(procedure? update) (update istyle)]
        [else (error who "bad style update: ~e" update)]))

;; ----------------------------------------

(define ((updater key default f) istyle)
  (hash-set istyle key (f (hash-ref istyle key default))))
(define ((updates . args) istyle) (apply hash-set* istyle args))
(define ((comp f g) x) (g (f x)))
(define ((iconser x) xs) (if (memq x xs) xs (cons x xs)))
(define ((toggler x) xs) (if (memq x xs) (remq x xs) (cons x xs)))

(define initial-stylemap
  (hash-union
   ;; ----------------------------------------
   ;; Standard Scribble style names

   ;; Content styles:
   (hash
    'emph (updater 'text-mods null (toggler 'italic))
    'tt '(text-base modern)
    'sf '(text-base swiss)
    'roman '(text-base roman)
    'larger (updater 'scale 1 (lambda (v) (* 3/2 v)))
    'smaller (updater 'scale 1 (lambda (v) (* 2/3 v)))
    "RktBlk" '(text-base modern white-space pre)
    "RktCmt" '(text-base modern color (#xC2 #x74 #x1F))
    "RktErr" (comp (updater 'text-mods null (iconser 'italic))
                   (updates 'text-base 'modern 'color "red"))
    "RktIn" '(text-base modern color (#xCC #x66 #x33))
    "RktKw" '(text-base modern color "black")
    "RktMeta" '(text-base modern color "black")
    "RktMod" '(text-base modern)
    "RktOut" '(text-base modern color (#x96 #x00 #x96))
    "RktOpt" (comp (updater 'text-mods null (iconser 'italic))
                   (updates 'color "black"))
    "RktPn" '(text-base modern color (#x84 #x3C #x24))
    "RktRes" '(text-base modern color (#x00 #x00 #xAF))
    "RktRdr" '(text-base modern)
    "RktSym" (list 'text-base 'modern
                   ;; Scribble renders in black, but DrRacket in dark blue.
                   ;; I think dark blue is a better contrast with slide text.
                   'color '(#x00 #x00 #x80))
    "RktVar" (comp (updater 'text-mods null (iconser 'italic))
                   (updates 'text-base 'modern 'color '(#x44 #x44 #x44)))
    "RktVal" '(text-base modern color (#x22 #x8B #x22))
    "RktInBG" '(bgcolor "lightgray")
    "defmodule" '(bgcolor (#xEB #xF0 #xF4))
    "highlighted" '(bgcolor (#xFF #xEE #xEE))
    "Rfiletitle" '(bgcolor (#xEE #xEE #xEE))
    "SCentered" '(block-halign center)
    'no-break '(white-space nowrap)
    'hspace '(text-base modern white-space pre))
   (let ([mods '(italic bold subscript superscript combine no-combine align unaligned)])
     (for/hash ([s (in-list mods)])
       (values s (updater 'text-mods null (iconser s)))))
   (for/hash ([s (in-list '("RktStxDef" "RktSymDef" "RktValDef"))])
     (values s '(text-base modern color "black" text-mods (bold))))
   (for/hash ([s (in-list '("RktValLink" "RktStxLink" "RktModLink"))])
     (values s '(color (#x00 #x77 #xAA))))

   ;; Block styles:
   (hash
    ;; paragraph:
    'author '()
    'pretitle '()
    'wraps '()

    ;; table:
    'boxed '(bgcolor "aliceblue" block-border (top) inset-to-width? #t)
    ;;     tables generally disable inset-to-width?, but a boxed table restores it
    'centered '(block-halign center) ;; ????
    'block '()

    ;; itemization:
    'compact (lambda (istyle) (hash-set istyle 'block-sep (get-line-sep istyle))) ;; generalized!
    'ordered '()

    ;; nested-flow:
    'inset '() ;; ignore
    'code-inset '(block-inset code) ;; FIXME: reduce width?
    'vertical-inset '(block-inset vertical)
    "refpara" '(block-halign right scale 3/4) ;; for margin-par

    ;; ie, "procedure", "syntax", etc in defproc, defform, etc
    "RBackgroundLabel" (list 'block-halign 'float-right 'inset-to-width? #f
                             'text-base 'modern 'color "darkgray" 'scale 2/3)

    )

   ;; Part styles:
   (hash
    'index '())
   ;; ----------------------------------------
   ;; Custom style names
   (hash
    'slide-title (list 'text-base TITLE-BASE 'text-size TITLE-SIZE 'color TITLE-COLOR))
   ))

(define (add-style-prop prop istyle [ignore-props null])
  (define (used v) (values #t v))
  (define (ignore? v) (member v ignore-props))
  (match prop
    [(text-post-property post)
     (used (hash-cons istyle 'text-post post))]
    [(elem-post-property post)
     (used (hash-cons istyle 'elem-post post))]
    [(s:color-property color)
     (used (hash-set istyle 'color (to-color color)))]
    [(s:background-color-property color)
     (used (hash-set istyle 'bgcolor (to-color color)))]
    [(? hash?) (used (merge-styles istyle prop))]
    [(? s:css-addition?) (used istyle)]
    [(? s:tex-addition?) (used istyle)]
    [(style-transformer f) (used (apply-update 'style-transformer f istyle))]
    ;; ignore 'tt-chars, 'omitable, 'never-indents, 'decorative ???
    [(? ignore?) (values #t istyle)]
    [_
     (when #f (log-scribble-slideshow-warning "add-style-prop: ignoring: ~e" prop))
     (values #f istyle)]))

;; ------------------------------------------------------------

(define (to-color color) color) ;; FIXME

(define (bg-colorize p c)
  (pin-under p 0 0 (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color c)))

;; ------------------------------------------------------------

(define current-sp-style
  (make-parameter (hash-set base-istyle 'styles initial-stylemap)))

(define current-istyle current-sp-style)
