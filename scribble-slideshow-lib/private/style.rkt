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

(define (hash-move h1 h2 keys)
  (for/fold ([h1 h1] [h2 h2]) ([key (in-list keys)])
    (cond [(hash-ref h1 key #f)
           => (lambda (v1)
                (values (hash-remove h1 key)
                        (cond [(hash-ref h2 key #f) h2]
                              [else (hash-set h2 key v1)])))]
          [else (values h1 h2)])))

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

;; Text styles:
;; - 'text-base : (U 'default font% (U 'roman ...) String) -- font face, see `text`
;; - 'text-size : Nat
;; - 'text-mods : (Listof PictTextStyleSymbol) -- see `text`
;; - 'text-post : (Listof (Pict -> Pict))

;; Content styles:
;; - 'color : (U #f String color%)
;; - 'bgcolor : (U #f String color%) -- fixme: replace with an elem-post?
;; - 'white-space : #f | 'pre | 'pre-wrap | 'nowrap
;; - 'elem-post : (Listof (Pict -> Pict))
;; - 'scale : NNReal

;; Para styles:
;; - 'justify : #t = 'justify | #f = 'greedy

;; Block style keys:
;; - 'bgcolor : (U color% String)
;; - 'block-width : ExtReal, if +inf.0 then no wrap & no inset block
;; - 'block-halign : (U 'left 'right 'center)
;; - 'block-sep : NNReal
;; - 'line-sep : NNReal

;; Accessors
(define (get-block-width istyle) (hash-ref istyle 'block-width BLOCK-WIDTH))
(define (get-block-sep istyle)   (hash-ref istyle 'block-sep   BLOCK-SEP))
(define (get-line-sep istyle)    (hash-ref istyle 'line-sep    LINE-SEP))
(define (get-code-inset) 0) ;; (/ (get-block-sep) 2)
(define (get-vertical-inset istyle) (get-line-sep istyle))

;; istyle-adjust-block-width : IStyle Real -> IStyle
(define (istyle-adjust-block-width istyle delta)
  (hash-update istyle 'block-width (lambda (v) (+ v delta)) BLOCK-WIDTH))

;; ============================================================
;; NStyle

;; An NStyle is an immutable hash mapping style keys (symbols) to values.

;; It represents style attributes that apply to the current node of the document
;; but not inherited by its children. Examples: itemization 'ordered mode, block
;; borders, block inset, etc.

;; Block style keys:
;; - 'block-border : (Listof (U 'all 'left 'right 'top 'bottom))
;; - 'block-margin : (Listof*4 Real)
;; - 'block-padding : (Listof*4 Real)
;; - 'bgcolor : (U String color%)

;; Itemization keys:
;; - 'itemization-mode : (U #f 'ordered)

;; Table keys:
;; - table-cell-styless : (Listof (Listof Style/#f)) or #f
;; - table-col-styles : (Listof Style/#f) or #f

;; Table cell keys:
;; - 'cell-border : (Listof (U 'all 'left 'right 'top 'bottom))
;; - 'cell-halign : (U 'left 'right 'center)
;; - 'cell-valign : (U 'top 'bottom 'vcenter 'baseline) -- currently ignored

;; Slide keys:
;; - layout   : 'auto | 'center | 'top | 'tall
;; - aspect   : 'widescreen | 'fullscreen | #f
;; - mode     : 'next | 'alt | #f | 'digress
;; - ignore   : 'ignore | 'ignore* | no-ignore | #f
;; - no-title : 'no-title | #f
;; - maker    : (-> Void)


;; ============================================================
;; StyleDiffs

;; A StyleDiffs is (Listof StyleDiff)
;; A StyleDiff is one of
;; - (list 'istyle { Symbol Value } ...)
;; - (list 'nstyle { Symbol Value } ...)
;; - (list 'update Symbol Value (Value -> Value))
;; - (list 'nstyle-update Symbol Value (Value -> Value))
;; - (list 'stylemap { Symbol/String Value } ...)
;; - (list 'ref (U String Symbol)) -- add effects of given style name
;; - (IStyle NStyle -> IStyle NStyle)

;; styles-update : IStyle NStyle StyleDiffs -> (values IStyle NStyle)
(define (styles-update istyle nstyle diffs)
  (for/fold ([istyle istyle] [nstyle nstyle])
            ([diff (in-list diffs)])
    (styles-update1 istyle nstyle diff)))

;; styles-update1 : IStyle NStyle StyleDiff -> (values IStyle NStyle)
(define (styles-update1 istyle nstyle diff)
  (match diff
    [(list* 'istyle kvs)
     (values (apply hash-set* istyle kvs) nstyle)]
    [(list* 'nstyle kvs)
     (values istyle (apply hash-set* nstyle kvs))]
    [(list 'update key default (? procedure? update))
     (values (hash-update istyle key update default) nstyle)]
    [(list 'nstyle-update key default (? procedure? update))
     (values istyle (hash-update nstyle key update default))]
    [(list 'stylemap kvs)
     (let ([stylemap (apply hash-set* (hash-ref istyle 'styles) kvs)])
       (values (hash-set istyle 'styles stylemap) nstyle))]
    [(list 'ref style-name)
     (cond [(istyle-stylemap-ref istyle style-name null)
            => (lambda (diffs)
                 (styles-update istyle nstyle diffs))]
           [else
            (log-scribble-slideshow-warning "styles-update1: undefined style ref: ~e" style-name)
            (values istyle nstyle)])]
    [(? procedure? update) (update istyle nstyle)]
    [_ (error 'styles-update1 "bad style diff: ~e" diff)]))

;; istyle-stylemap-ref : IStyle Any Default -> (U StyleDiffs Default)
;; Lookup a style name (string or symbol) or named handler hash.
(define (istyle-stylemap-ref istyle style-name [default null])
  (define stylemap (hash-ref istyle 'styles))
  (hash-ref stylemap style-name default))


;; ============================================================
;; Interpreting Scribble Styles

;; Style properties
(struct text-post-property (post))
(struct elem-post-property (post))
(struct style-transformer (f)) ;; FIXME: delete!!!!
(struct style-diffs (diffs))

(define-values (prop:sp-style-prop sp-style-prop? sp-style-prop-ref)
  (make-struct-type-property 'scribble-pict-style-property))

;; add*-style : Style IStyle NStyle -> (values IStyle NStyle)
(define (add*-style s istyle [nstyle #hasheq()] #:kind [kind #f])
  (define handlers (istyle-stylemap-ref istyle `(handlers ,kind) #hasheq()))
  (match s
    [(s:style name props)
     (define-values (istyle1 nstyle1)
       (add*-simple-style name istyle nstyle #:handlers handlers))
     (for/fold ([istyle istyle1] [nstyle nstyle1])
               ([prop (in-list props)])
       (add*-style-prop prop istyle nstyle #:handlers handlers))]
    [s (add*-simple-style s istyle nstyle #:handlers handlers)]))

;; add*-simple-style : (U #f Symbol String) IStyle NStyle -> (values IStyle NStyle)
(define (add*-simple-style style-name istyle nstyle #:handlers handlers)
  (cond [(eq? style-name #f) (values istyle nstyle)]
        [(istyle-stylemap-ref istyle style-name #f)
         => (lambda (diffs) (styles-update istyle nstyle diffs))]
        [(hash-ref handlers style-name #f)
         => (lambda (diffs) (styles-update istyle nstyle diffs))]
        [else
         (when #t (log-scribble-slideshow-warning "add-style: ignoring: ~e" style-name))
         (values istyle nstyle)]))

;; add*-style-prop : Any IStyle NStyle -> (values IStyle NStyle)
(define (add*-style-prop prop istyle nstyle #:handlers handlers)
  (match prop
    [(text-post-property post)
     (values (hash-cons istyle 'text-post post) nstyle)]
    [(elem-post-property post)
     (values (hash-cons istyle 'elem-post post) nstyle)]
    [(s:color-property color)
     (values (hash-set istyle 'color (to-color color)) nstyle)]
    [(s:background-color-property color)
     (values (hash-set istyle 'bgcolor (to-color color)) nstyle)]
    [(s:table-cells cell-styless)
     (values istyle (hash-set nstyle 'table-cell-styless cell-styless))]
    [(s:table-columns col-styles)
     (values istyle (hash-set nstyle 'table-col-styles col-styles))]
    [(style-diffs diffs)
     (styles-update istyle nstyle diffs)]
    [(style-transformer f) ;; FIXME: delete!
     (values (f istyle) nstyle)]
    [(? sp-style-prop?)
     ((sp-style-prop-ref prop) prop istyle nstyle)]
    [(? s:css-addition?) (values istyle nstyle)]
    [(? s:tex-addition?) (values istyle nstyle)]
    ;; ----
    [_
     (cond [(hash-ref handlers prop #f)
            => (lambda (diffs) (styles-update istyle nstyle diffs))]
           [(istyle-stylemap-ref istyle prop #f)
            => (lambda (diffs) (styles-update istyle nstyle diffs))]
           [else
            (when #f (log-scribble-slideshow-warning "add*-style-prop: ignoring: ~e" prop))
            (values istyle (hash-cons nstyle 'unhandled prop))])]))

;; ============================================================

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
  `#hasheq(;; Text Styles
           (text-base       . default)
           (text-size       . ,BASE-SIZE)
           (text-mods       . ())
           (text-post       . ())
           ;; Content Styles
           (color           . #f)
           (white-space     . #f)
           (elem-post       . ())
           (scale           . 1)
           ;; Para Styles
           (justify         . #f)
           ;; Block Styles
           (bgcolor         . #f)
           (block-halign    . left)
           (block-width     . ,BLOCK-WIDTH)
           (block-sep       . ,BLOCK-SEP)
           (line-sep        . ,LINE-SEP)
           ))

(define wide-istyle (hash-set base-istyle 'block-width WIDE-BLOCK-WIDTH))

;; ----------------------------------------

(define ((iconser x) xs) (if (memq x xs) xs (cons x xs)))
(define ((toggler x) xs) (if (memq x xs) (remq x xs) (cons x xs)))

(define initial-stylemap
  (hash
   ;; ----------------------------------------
   ;; Standard content style names:

   'italic      `((update text-mods ,null ,(iconser 'italic)))
   'bold        `((update text-mods ,null ,(iconser 'bold)))
   'subscript   `((update text-mods ,null ,(iconser 'subscript)))
   'superscript `((update text-mods ,null ,(iconser 'superscript)))
   'combine     `((update text-mods ,null ,(iconser 'combine)))
   'no-combine  `((update text-mods ,null ,(iconser 'no-combine)))
   'align       `((update text-mods ,null ,(iconser 'align)))
   'unaligned   `((update text-mods ,null ,(iconser 'unaligned)))

   'emph     `((update text-mods ,null ,(toggler 'italic)))
   'tt       `((istyle text-base modern))
   'sf       `((istyle text-base swiss))
   'roman    `((istyle text-base roman))
   'larger   `((update scale 1 ,(lambda (v) (* 3/2 v))))
   'smaller  `((update scale 1 ,(lambda (v) (* 2/3 v))))
   "RktBlk"  `((istyle text-base modern white-space pre))
   "RktCmt"  `((istyle text-base modern color (#xC2 #x74 #x1F)))
   "RktErr"  `((ref italic) (istyle text-base modern color "red"))
   "RktIn"   `((istyle text-base modern color (#xCC #x66 #x33)))
   "RktKw"   `((istyle text-base modern color "black"))
   "RktMeta" `((istyle text-base modern color "black"))
   "RktMod"  `((istyle text-base modern))
   "RktOut"  `((istyle text-base modern color (#x96 #x00 #x96)))
   "RktOpt"  `((ref italic) (istyle 'color "black"))
   "RktPn"   `((istyle text-base modern color (#x84 #x3C #x24)))
   "RktRes"  `((istyle text-base modern color (#x00 #x00 #xAF)))
   "RktRdr"  `((istyle text-base modern))
   "RktSym"  `((istyle text-base modern
                       ;; Scribble renders in black, but DrRacket in dark blue.
                       ;; I think dark blue is a better contrast with slide text.
                       color (#x00 #x00 #x80)))
   "RktVar"  `((ref italic) (istyle text-base modern color (#x44 #x44 #x44)))
   "RktVal"  `((istyle text-base modern color (#x22 #x8B #x22)))
   "RktInBG" `((istyle bgcolor "lightgray"))
   "defmodule"   `((istyle bgcolor (#xEB #xF0 #xF4)))
   "highlighted" `((istyle bgcolor (#xFF #xEE #xEE)))
   "Rfiletitle"  `((istyle bgcolor (#xEE #xEE #xEE)))
   "SCentered"   `((istyle block-halign center))
   'no-break `((istyle white-space nowrap))
   'hspace   `((istyle text-base modern white-space pre))

   "Rkt*Def"    `((istyle text-base modern color "black" text-mods (bold)))
   "RktStxDef"  `((ref "Rkt*Def"))
   "RktSymDef"  `((ref "Rkt*Def"))
   "RktValDef"  `((ref "Rkt*Def"))

   "Rkt*Link"   `((istyle color (#x00 #x77 #xAA)))
   "RktValLink" `((ref "Rkt*Link"))
   "RktStxLink" `((ref "Rkt*Link"))
   "RktModLink" `((ref "Rkt*Link"))

   ;; ----------------------------------------
   ;; Standard block styles:

   ;; 'compact generalized from itemization to all block forms
   'compact `(,(lambda (istyle nstyle)
                 (values (hash-set istyle 'block-sep (get-line-sep istyle)) nstyle)))

   ;; ----------------------------------------
   ;; Custom styles:
   'slide-title `((istyle text-base ,TITLE-BASE text-size ,TITLE-SIZE color ,TITLE-COLOR))

   ;; ----------------------------------------
   ;; Default Property Handlers
   '(handlers paragraph)
   (hash
    'author    '()
    'pretitle  '()
    'wraps     '()
    'omitable  '()
    'div       '()
    'never-indents '())

   '(handlers itemization)
   (hash
    'ordered   `((nstyle itemization-mode ordered))
    'never-indents '())

   '(handlers nested-flow)
   (hash
    'inset          `((nstyle block-margin (,LINE-SEP 0 ,LINE-SEP 0)))
    'code-inset     `((nstyle block-margin (,LINE-SEP 0 ,LINE-SEP 0)))
    'vertical-inset `((nstyle block-margin (0 ,LINE-SEP 0 ,LINE-SEP)))
    ;; for margin-par:
    "refpara" `((istyle block-halign right scale 3/4))
    ;; ie, "procedure", "syntax", etc in defproc, defform, etc
    "RBackgroundLabel" `((nstyle float right)
                         (istyle block-halign right text-base modern color "darkgray"))
    'command '()
    'multicommand '()
    'never-indents '()
    'decorative '()
    'pretitle '())

   '(handlers compound-paragraph)
   (hash
    'command '()
    'never-indents '())

   '(handlers table)
   (hash
    'boxed     '((nstyle table-full-width #t bgcolor "aliceblue" block-border (top)))
    'centered  `((istyle block-halign center))
    'aux       '()
    'block     '()
    'never-indents '())

   '(handlers table-cell)
   (hash
    'left       `((nstyle cell-halign left))
    'right      `((nstyle cell-halign right))
    'center     `((nstyle cell-halign center))
    'top        `((nstyle cell-valign top))
    'bottom     `((nstyle cell-valign bottom))
    'vcenter    `((nstyle cell-valign vcenter))
    'baseline   `((nstyle cell-valign baseline))
    'border         `((nstyle-update cell-border () ,(iconser 'all)))
    'left-border    `((nstyle-update cell-border () ,(iconser 'left)))
    'right-border   `((nstyle-update cell-border () ,(iconser 'right)))
    'top-border     `((nstyle-update cell-border () ,(iconser 'top)))
    'bottom-border  `((nstyle-update cell-border () ,(iconser 'bottom))))

   '(handlers part)
   (hash
    'index '())

   '(handlers slide)
   (hash
    'widescreen `((nstyle slide-aspect widescreen))
    'fullscreen `((nstyle slide-aspect fullscreen))

    'auto     `((nstyle slide-layout auto))
    'center   `((nstyle slide-layout center))
    'top      `((nstyle slide-layout top))
    'tall     `((nstyle slide-layout tall))

    'next     `((nstyle slide-mode next))
    'alt      `((nstyle slide-mode alt))
    'digress  `((nstyle slide-mode digress))

    'ignore   `((nstyle slide-ignore ignore))
    'ignore*  `((nstyle slide-ignore ignore*))
    'no-ignore `((nstyle slide-ignore no-ignore))

    'no-title `((nstyle slide-no-title no-title)))))

;; ------------------------------------------------------------

(define (to-color color) color) ;; FIXME

(define (bg-colorize p c)
  (pin-under p 0 0 (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color c)))

;; ------------------------------------------------------------

(define current-sp-style
  (make-parameter (hash-set base-istyle 'styles initial-stylemap)))

(define current-istyle current-sp-style)
