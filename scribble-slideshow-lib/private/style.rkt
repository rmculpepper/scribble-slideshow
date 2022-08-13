;; Copyright 2019-2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/hash
         racket/class
         (only-in racket/draw color% make-color)
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
;; - 'text-mods : (Listof PictTextStyleSymbol)  -- see `text`
;; - 'text-post : (Listof (Pict -> Pict))       -- dimension-preserving!

;; Content styles:
;; - 'color : (U #f String color%)
;; - 'bgcolor : (U #f String color%)
;; - 'white-space : #f | 'pre | 'pre-wrap | 'nowrap
;; - 'elem-post : (Listof (Pict -> Pict))       -- dimension-preserving!
;; - 'scale : NNReal

;; Para styles:
;; - 'justify : #t = 'justify | #f = 'greedy

;; Itemization styles:
;; - 'itemize-bullets : (Listof Symbol)

;; Block style keys:
;; - 'bgcolor : (U color% String)
;; - 'block-width : ExtReal, if +inf.0 then no wrap & no inset block
;; - 'block-halign : (U 'left 'right 'center)
;; - 'block-sep : NNReal
;; - 'line-sep : NNReal

;; Context style keys:
;; - 'itemize-level : Nat
;; - 'table-width : ExtReal

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
;; - 'block-post : (Listof (Pict -> Pict))  -- dimension-preserving!

;; Itemization keys:
;; - 'itemization-mode : (U #f 'ordered)

;; Table keys:
;; - table-cell-styless : (Listof (Listof Style/#f)) or #f
;; - table-col-styles : (Listof Style/#f) or #f
;; - table-full-width : Boolean

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

;; Accessors
(define (get-block-margins nstyle)
  (to-ltrb 'get-block-margins (or (hash-ref nstyle 'block-margin #f) 0)))
(define (get-block-padding nstyle)
  (to-ltrb 'get-block-padding (or (hash-ref nstyle 'block-padding #f) 0)))

(define (to-ltrb who v)
  (match v
    [(list l t r b) (values l t r b)]
    [(list lr tb) (values lr tb lr tb)]
    [(? real? n) (values n n n n)]
    [_ (error who "bad value: ~e" v)]))

;; ============================================================
;; StyleDiffs

;; A StyleDiffs is (Listof StyleDiff)
;; A StyleDiff is one of
;; - (list 'iset { Symbol Value } ...)
;; - (list 'nset { Symbol Value } ...)
;; - (list 'iset* UpdateDiff ...)
;; - (list 'nset* UpdateDiff ...)
;; - (list 'ref (U String Symbol)) -- add effects of given style name
;; - (list 'stylemap { Symbol/String Value } ...)
;; - (IStyle NStyle -> IStyle NStyle)

;; An UpdateDiff is one of
;; - (list 'set Symbol Value)
;; - (list 'iadd Symbol Value)
;; - (list 'toggle Symbol Value)
;; - (list 'update Symbol Value (Value -> Value))
;; - (list 'setf Symbol (Hash -> Value))

;; styles-update : IStyle NStyle StyleDiffs -> (values IStyle NStyle)
(define (styles-update istyle nstyle diffs)
  (for/fold ([istyle istyle] [nstyle nstyle])
            ([diff (in-list diffs)])
    (styles-update1 istyle nstyle diff)))

;; styles-update1 : IStyle NStyle StyleDiff -> (values IStyle NStyle)
(define (styles-update1 istyle nstyle diff)
  (match diff
    [(list* 'iset kvs)
     (values (apply hash-set* istyle kvs) nstyle)]
    [(list* 'nset kvs)
     (values istyle (apply hash-set* nstyle kvs))]
    [(list* 'iset* upds)
     (values (foldl styles-update1/upd istyle upds) nstyle)]
    [(list* 'nset* upds)
     (values istyle (foldl styles-update1/upd nstyle upds))]
    [(list* 'stylemap kvs)
     (let ([stylemap (apply hash-set* (hash-ref istyle 'styles) kvs)])
       (values (hash-set istyle 'styles stylemap) nstyle))]
    [(list 'ref style-name)
     (cond [(istyle-stylemap-ref istyle style-name null)
            => (lambda (diffs) (styles-update istyle nstyle diffs))]
           [else
            (log-scribble-slideshow-warning "styles-update1: undefined style ref: ~e" style-name)
            (values istyle nstyle)])]
    [(? procedure? update) (update istyle nstyle)]
    [_ (error 'styles-update1 "bad style diff: ~e" diff)]))

;; styles-update1/upd : UpdateDiff [IN]Style -> [IN]Style
(define (styles-update1/upd upd instyle)
  (match upd
    [(list 'set key value)
     (hash-set instyle key value)]
    [(list 'push key value)
     (let ([vs (hash-ref instyle key null)])
       (cond [(member value vs) instyle]
             [else (hash-set instyle key (cons value vs))]))]
    [(list 'toggle key value)
     (let ([vs (hash-ref instyle key null)])
       (cond [(member value vs) (hash-set instyle (remove value vs))]
             [else (hash-set instyle key (cons value vs))]))]
    [(list 'remove key value)
     (let ([vs (hash-ref instyle key null)])
       (cond [(member value vs) (hash-set instyle (remove value vs))]
             [else instyle]))]
    [(list 'update key default (? procedure? update))
     (hash-update instyle key update default)]
    [(list 'setf key get-value)
     (hash-set instyle key (get-value instyle))]
    [_ (error 'styles-update1/upd "bad style-diff update: ~e" upd)]))

;; istyle-stylemap-ref : IStyle Any Default -> (U StyleDiffs Default)
;; Lookup a style name (string or symbol) or named handler hash.
(define (istyle-stylemap-ref istyle style-name [default null])
  (define stylemap (hash-ref istyle 'styles))
  (hash-ref stylemap style-name default))


;; ============================================================
;; Interpreting Scribble Styles

;; prop:sp-style-prop : prop of (Self IStyle NStyle -> (values IStyle NStyle))
(define-values (prop:sp-style-prop sp-style-prop? sp-style-prop-ref)
  (make-struct-type-property 'scribble-pict-style-property))

;; Style properties
(struct style-diffs (diffs)
  #:property prop:sp-style-prop
  (lambda (self istyle nstyle)
    (styles-update istyle nstyle (style-diffs-diffs self))))

;; style+ : StyleLike List -> Style
(define (style+ s props)
  (match s
    [(s:style style-name props0)
     (s:style style-name (if (null? props) props0 (append props0 props)))]
    [(? symbol?) (s:style s props)]
    [(? string?) (s:style s props)]
    [(? sp-style-prop?) (s:style #f (cons s props))]
    [#f (if (null? props) s:plain (s:style #f props))]))

;; add*-style : Style IStyle NStyle -> (values IStyle NStyle)
(define (add*-style s istyle [nstyle #hasheq()] #:kind [kind #f])
  (define handlers (istyle-stylemap-ref istyle `(handlers ,kind) #hasheq()))
  (match s
    [(s:style name props)
     (define-values (istyle1 nstyle1)
       (add*-style1 name istyle nstyle #:handlers handlers))
     (for/fold ([istyle istyle1] [nstyle nstyle1])
               ([prop (in-list props)])
       (add*-style1 prop istyle nstyle #:handlers handlers))]
    [s (add*-style1 s istyle nstyle #:handlers handlers)]))

;; add*-style1 : Any IStyle NStyle -> (values IStyle NStyle)
(define (add*-style1 v istyle nstyle #:handlers handlers)
  (match v
    [#f (values istyle nstyle)]
    [(? (lambda (v) (or (symbol? v) (string? v))))
     (cond [(or (hash-ref handlers v #f)
                (istyle-stylemap-ref istyle v #f))
            => (lambda (diffs) (styles-update istyle nstyle diffs))]
           [else (values istyle (hash-cons nstyle 'unhandled v))])]
    [(? sp-style-prop?)
     ((sp-style-prop-ref v) v istyle nstyle)]
    [(s:color-property color)
     (values (hash-set istyle 'color (to-color color)) nstyle)]
    [(s:background-color-property color)
     (values (hash-set istyle 'bgcolor (to-color color)) nstyle)]
    [(s:table-cells cell-styless)
     (values istyle (hash-set nstyle 'table-cell-styless cell-styless))]
    [(s:table-columns col-styles)
     (values istyle (hash-set nstyle 'table-col-styles col-styles))]
    [(? s:css-addition?) (values istyle nstyle)]
    [(? s:tex-addition?) (values istyle nstyle)]
    ;; ----
    [_ (values istyle (hash-cons nstyle 'unhandled v))]))

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

(define initial-stylemap
  (hash
   ;; ----------------------------------------
   ;; Common style hooks:

   'modernfont `((iset text-base modern))
   'romanfont  `((iset text-base roman))
   'swissfont  `((iset text-base swiss))

   ;; ----------------------------------------
   ;; Standard content style names:

   'italic      `((iset* (push text-mods italic)))
   'bold        `((iset* (push text-mods bold)))
   'subscript   `((iset* (push text-mods subscript)   (remove text-mods superscript)))
   'superscript `((iset* (push text-mods superscript) (remove text-mods subscript)))
   'combine     `((iset* (push text-mods combine)     (remove text-mods no-combine)))
   'no-combine  `((iset* (push text-mods no-combine)  (remove text-mods combine)))
   'align       `((iset* (push text-mods align)       (remove text-mods unaligned)))
   'unaligned   `((iset* (push text-mods unaligned)   (remove text-mods align)))

   'emph     `((iset* (toggle text-mods italic)))
   'tt       `((ref modernfont))
   'sf       `((ref swissfont))
   'roman    `((ref romanfont))
   'larger   `((iset* (update scale 1 ,(lambda (v) (* 3/2 v)))))
   'smaller  `((iset* (update scale 1 ,(lambda (v) (* 2/3 v)))))
   "RktBlk"  `((ref modernfont) (iset white-space pre))
   "RktCmt"  `((ref modernfont) (iset color (#xC2 #x74 #x1F)))
   "RktErr"  `((ref modernfont) (ref italic) (iset color "red"))
   "RktIn"   `((ref modernfont) (iset color (#xCC #x66 #x33)))
   "RktKw"   `((ref modernfont) (iset color "black"))
   "RktMeta" `((ref modernfont) (iset color "black"))
   "RktMod"  `((ref modernfont))
   "RktOut"  `((ref modernfont) (iset color (#x96 #x00 #x96)))
   "RktOpt"  `((ref italic) (iset color "black"))
   "RktPn"   `((ref modernfont) (iset color (#x84 #x3C #x24)))
   "RktRes"  `((ref modernfont) (iset color (#x00 #x00 #xAF)))
   "RktRdr"  `((ref modernfont))
   "RktSym"  `((ref modernfont)
               ;; Scribble renders in black, but DrRacket in dark blue.
               ;; I think dark blue is a better contrast with slide text.
               (iset color (#x00 #x00 #x80)))
   "RktVar"  `((ref modernfont) (ref italic) (iset color (#x44 #x44 #x44)))
   "RktVal"  `((ref modernfont) (iset color (#x22 #x8B #x22)))
   "RktInBG" `((iset bgcolor "lightgray"))
   "defmodule"   `((iset bgcolor (#xEB #xF0 #xF4)))
   "highlighted" `((iset bgcolor (#xFF #xEE #xEE)))
   "Rfiletitle"  `((iset bgcolor (#xEE #xEE #xEE)))
   "SCentered"   `((iset block-halign center))
   'no-break `((iset white-space nowrap))
   'hspace   `((ref modernfont) (iset white-space pre))

   "Rkt*Def"    `((ref modernfont) (iset color "black" text-mods (bold)))
   "RktStxDef"  `((ref "Rkt*Def"))
   "RktSymDef"  `((ref "Rkt*Def"))
   "RktValDef"  `((ref "Rkt*Def"))

   "Rkt*Link"   `((iset color (#x00 #x77 #xAA)))
   "RktValLink" `((ref "Rkt*Link"))
   "RktStxLink" `((ref "Rkt*Link"))
   "RktModLink" `((ref "Rkt*Link"))

   ;; ----------------------------------------
   ;; Standard block styles:

   ;; 'compact generalized from itemization to all block forms
   'compact `((iset* (setf block-sep ,(lambda (istyle) (get-line-sep istyle)))))

   ;; ----------------------------------------
   ;; Custom styles:
   'slide-title `((iset text-base ,TITLE-BASE text-size ,TITLE-SIZE color ,TITLE-COLOR))

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
    'ordered   `((nset itemization-mode ordered))
    'never-indents '())

   '(handlers nested-flow)
   (hash
    'inset          `((nset block-margin (,LINE-SEP 0 ,LINE-SEP 0)))
    'code-inset     `((nset block-margin (,LINE-SEP 0 ,LINE-SEP 0)))
    'vertical-inset `((nset block-margin (0 ,LINE-SEP 0 ,LINE-SEP)))
    ;; for margin-par:
    "refpara" `((iset block-halign right scale 3/4))
    ;; ie, "procedure", "syntax", etc in defproc, defform, etc
    "RBackgroundLabel" `((iset block-halign right color "darkgray")
                         (ref modernfont) (nset float right))
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
    'boxed     `((nset table-full-width #t bgcolor "aliceblue" block-border (top)))
    'centered  `((iset block-halign center))
    'aux       '()
    'block     '()
    'never-indents '())

   '(handlers table-cell)
   (hash
    'left       `((nset cell-halign left))
    'right      `((nset cell-halign right))
    'center     `((nset cell-halign center))
    'top        `((nset cell-valign top))
    'bottom     `((nset cell-valign bottom))
    'vcenter    `((nset cell-valign vcenter))
    'baseline   `((nset cell-valign baseline))
    'border         `((nset* (push cell-border all)))
    'left-border    `((nset* (push cell-border left)))
    'right-border   `((nset* (push cell-border right)))
    'top-border     `((nset* (push cell-border top)))
    'bottom-border  `((nset* (push cell-border bottom))))

   '(handlers part)
   (hash
    'index '())

   '(handlers layer)
   (hash)

   '(handlers slide)
   (hash
    'widescreen `((nset slide-aspect widescreen))
    'fullscreen `((nset slide-aspect fullscreen))

    'auto     `((nset slide-layout auto))
    'center   `((nset slide-layout center))
    'top      `((nset slide-layout top))
    'tall     `((nset slide-layout tall))

    'next     `((nset slide-mode next))
    'alt      `((nset slide-mode alt))
    'digress  `((nset slide-mode digress))

    'ignore   `((nset slide-ignore ignore))
    'ignore*  `((nset slide-ignore ignore*))
    'no-ignore `((nset slide-ignore no-ignore))

    'no-title `((nset slide-no-title no-title)))))

;; ------------------------------------------------------------

(define (to-color c)
  (cond [(is-a? c color%) c]
        [(string? c) c]
        [(list? c) (apply make-color c)]
        [else (error 'to-color "bad color: ~e" c)]))

(define (bg-colorize p c)
  (pin-under p 0 0
             (filled-rectangle (pict-width p) (pict-height p)
                               #:draw-border? #f #:color (to-color c))))

;; ------------------------------------------------------------

(define current-sp-style
  (make-parameter (hash-set base-istyle 'styles initial-stylemap)))

(define current-istyle current-sp-style)
