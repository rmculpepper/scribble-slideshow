;; Copyright 2019-2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
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
  `#hasheq(;; Slide styles
           (slide-title-color . ,TITLE-COLOR)
           (slide-title-size  . ,TITLE-SIZE)
           (slide-title-base  . ,TITLE-BASE)

           ;; Block Styles
           (inset-to-width? . #t)
           (block-width     . ,BLOCK-WIDTH)
           (block-sep       . ,BLOCK-SEP)
           (line-sep        . ,LINE-SEP)
           ;; Elem Styles
           (text-base       . default)
           (text-size       . ,BASE-SIZE)
           (scale           . 1)))

(define wide-istyle (hash-set base-istyle 'block-width WIDE-BLOCK-WIDTH))

(define current-sp-style (make-parameter base-istyle))
(define current-istyle current-sp-style)

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

(define (add-style s istyle)
  (match s
    [(s:style name props)
     (foldl add-style-prop (add-style name istyle) props)]
    [s (add-simple-style s istyle)]))

(define (add-simple-style s istyle)
  (case s
    [(italic bold subscript superscript #||# combine no-combine aligned unaligned)
     (hash-cons istyle 'text-mods s)]
    [(emph)
     (let ([b (hash-ref istyle 'text-mods null)])
       (hash-set istyle 'text-mods (if (memq 'italic b) (remq 'italic b) (cons 'italic b))))]
    [(tt) (hash-set istyle 'text-base 'modern)]
    [(sf) (hash-set istyle 'text-base 'swiss)]
    [(roman) (hash-set istyle 'text-base s)]
    [(larger) (hash-set istyle 'scale (* 3/2 (hash-ref istyle 'scale 1)))]
    [(smaller) (hash-set istyle 'scale (* 2/3 (hash-ref istyle 'scale 1)))]
    [("RktBlk") (hash-set* istyle 'text-base 'modern 'white-space 'pre)]
    [("RktCmt") (hash-set* istyle 'text-base 'modern 'color '(#xC2 #x74 #x1F))]
    [("RktErr") (hash-set* (hash-cons istyle 'text-mods 'italic)
                           'text-base 'modern 'color "red")]
    [("RktIn") (hash-set* istyle 'text-base 'modern 'color '(#xCC #x66 #x33))]
    [("RktKw") (hash-set* istyle 'text-base 'modern 'color "black")]
    [("RktMeta") (hash-set* istyle 'text-base 'modern 'color "black")]
    [("RktMod") (hash-set* istyle 'text-base 'modern)]
    [("RktOut") (hash-set* istyle 'text-base 'modern 'color '(#x96 #x00 #x96))]
    [("RktOpt") (hash-set* (hash-cons istyle 'text-mods 'italic) 'color "black")]
    [("RktPn") (hash-set* istyle 'text-base 'modern 'color '(#x84 #x3C #x24))]
    [("RktRes") (hash-set* istyle 'text-base 'modern 'color '(#x00 #x00 #xAF))]
    [("RktRdr") (hash-set* istyle 'text-base 'modern)]
    [("RktSym") (hash-set* istyle 'text-base 'modern
                           ;; Scribble renders in black, but DrRacket in dark blue.
                           ;; I think dark blue is a better contrast with slide text.
                           'color '(#x00 #x00 #x80))]
    [("RktVar") (hash-set* (hash-cons istyle 'text-mods 'italic)
                           'text-base 'modern 'color '(#x44 #x44 #x44))]
    [("RktVal") (hash-set* istyle 'text-base 'modern 'color '(#x22 #x8B #x22))]
    [("RktInBG") (hash-set istyle 'bgcolor "lightgray")]
    [("RktStxDef" "RktSymDef" "RktValDef")
     (hash-set* istyle 'text-base 'modern 'color "black" 'text-mods '(bold))]
    [("RktValLink" "RktStxLink" "RktModLink")
     (hash-set* istyle 'color '(#x00 #x77 #xAA))]
    [("defmodule") (hash-set istyle 'bgcolor '(#xEB #xF0 #xF4))]
    [("highlighted") (hash-set istyle 'bgcolor '(#xFF #xEE #xEE))]
    [("Rfiletitle") (hash-set istyle 'bgcolor '(#xEE #xEE #xEE))]
    [("SCentered") (hash-set istyle 'block-halign 'center)]
    [(hspace) (hash-set* istyle 'text-base 'modern 'white-space 'pre)]
    [(#f) istyle]
    [else
     (log-scribble-slideshow-warning "add-style: ignoring: ~e" s)
     istyle]))

(define (add-style-prop prop istyle)
  (match prop
    [(text-post-property post)
     (hash-cons istyle 'text-post post)]
    [(elem-post-property post)
     (hash-cons istyle 'elem-post post)]
    [(? hash?)
     (for/fold ([istyle istyle]) ([(k v) (in-hash prop)]) (hash-set istyle k v))]
    [(s:color-property color)
     (hash-set istyle 'color (to-color color))]
    [(s:background-color-property color)
     (hash-set istyle 'bgcolor (to-color color))]
    [(? s:css-addition?) istyle]
    [(? s:tex-addition?) istyle]
    [(style-transformer f) (f istyle)]
    ['tt-chars istyle]
    [(or 'omitable 'never-indents 'decorative) istyle] ;; FIXME?
    [_
     (log-scribble-slideshow-warning "add-style-prop: ignoring: ~e" prop)
     istyle]))

;; ------------------------------------------------------------

(define (to-color color) color) ;; FIXME

(define (bg-colorize p c)
  (pin-under p 0 0 (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color c)))
