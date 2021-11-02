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

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
(define (hash-remove* h ks) (for/fold ([h h]) ([k (in-list ks)]) (hash-remove h k)))

(define-logger scribble-slideshow)

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
;; - 'keep-whitespace? : Boolean
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
    [("SCentered") (hash-set istyle 'block-halign 'center)]
    [("RktInBG") (hash-set istyle 'bgcolor "lightgray")]
    [("RktIn") (hash-set* istyle 'text-base 'modern 'color '(#xCC #x66 #x33))]
    [("RktPn") (hash-set* istyle 'text-base 'modern 'color '(#x84 #x3C #x24))]
    [("RktSym") (hash-set* istyle 'text-base 'modern 'color '(#x00 #x00 #x80))] ;; ???
    [("RktVar") (hash-set* (hash-cons istyle 'text-mods 'italic)
                           'text-base 'modern 'color '(#x40 #x40 #x40))]
    [("RktRes") (hash-set* istyle 'text-base 'modern 'color '(#x00 #x00 #xAF))]
    [("RktOut") (hash-set* istyle 'text-base 'modern 'color '(#x96 #x00 #x96))]
    [("RktCmt") (hash-set* istyle 'text-base 'modern 'color '(#xC2 #x74 #x1F))]
    [("RktVal") (hash-set* istyle 'text-base 'modern 'color '(#x22 #x8B #x22))]
    [("RktBlk") (hash-set* istyle 'text-base 'modern 'keep-whitespace? #t)]
    [("RktSymDef") (hash-set* istyle 'text-base 'modern 'color "black" 'text-mods '(bold))]
    [(hspace) (hash-set* istyle 'text-base 'modern 'keep-whitespace? #t)]
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

(define (to-color color) color) ;; FIXME

;; ------------------------------------------------------------
;; Block Styles

;; Block style keys:
;; - 'bgcolor : (U color% String)
;; - 'inset-to-width? : Boolean
;; - 'block-width : PositiveReal
;; - 'block-halign : (U 'left 'right 'center)
;; - 'block-border : (Listof (U 'all 'left 'right 'top 'bottom))
;; - 'block-inset : (U 'code 'vertical)

;; A RenderedBlock is one of
;; - Pict
;; - (cons (U #f Pict) Pict)

(define (add-block-style s istyle)
  (match s
    [(s:style name props)
     (foldl add-block-style-prop (add-block-style name istyle) props)]
    ;; ----
    ;; Special case: tables generally disable inset-to-width?, but a boxed table restores it.
    ['boxed (hash-set* istyle 'bgcolor "aliceblue" 'block-border '(top) 'inset-to-width? #t)]
    ;; ----
    ['vertical-inset (hash-set* istyle 'block-inset 'vertical)]
    ['code-inset (hash-set* istyle 'block-inset 'code)] ;; FIXME: reduce width?
    ["RBackgroundLabel" ;; ie, "procedure", "syntax" etc in defproc, defform, etc
     (hash-set* istyle 'block-halign 'float-right 'inset-to-width? #f
                'text-base 'modern 'color "darkgray" 'scale 2/3)]
    ["refpara" ;; style on nested-flow for margin-par
     (hash-set* istyle 'block-halign 'right 'scale 3/4)]
    ["SCentered" (hash-set istyle 'block-halign 'center)]
    ;; ----
    [#f istyle]
    [_
     (log-scribble-slideshow-warning "add-block-style: ignoring: ~e" s)
     istyle]))

(define (add-block-style-prop prop istyle)
  (match prop
    [(or 'omitable 'never-indents) istyle]
    ['decorative istyle] ;; FIXME?
    [prop (add-style-prop prop istyle)]))

(define (remove-block-styles istyle)
  (hash-remove* istyle '(bgcolor block-halign block-border block-inset)))

;; apply-block-styles : IStyle RenderedBlock -> RenderedBlock
(define (apply-block-styles istyle p)
  (cond [(pair? p) (cons (and (car p) (apply-block-styles* istyle (car p))) (cdr p))]
        [else (apply-block-styles* istyle p)]))

(define (apply-block-styles* istyle p)
  (let* ([p (cond [(hash-ref istyle 'inset-to-width? #f)
                   (define dwidth (- (hash-ref istyle 'block-width) (pict-width p)))
                   (case (hash-ref istyle 'block-halign 'left)
                     [(left) (inset p 0 0 dwidth 0)]
                     [(right) (inset p dwidth 0 0 0)]
                     [(center) (inset p (/ dwidth 2) 0 (/ dwidth 2) 0)]
                     [(float-right) p])]
                  [else p])]
         [p (cond [(hash-ref istyle 'bgcolor #f)
                   => (lambda (c) (bg-colorize p c))]
                  [else p])]
         [p (cond [(hash-ref istyle 'block-border #f)
                   => (lambda (borders) (add-borders p borders))]
                  [else p])]
         [p (case (hash-ref istyle 'block-inset #f)
              [(code) (inset p (get-code-inset) 0)]
              [(vertical) (inset p 0 (get-vertical-inset istyle))]
              [else p])]
         [p (case (and (hash-ref istyle 'block-halign #f))
              [(float-right) (cons #f p)]
              [else p])])
    p))

;; append-blocks : PositiveReal (Listof RenderedBlock) -> RenderedBlock
(define (append-blocks sep ps)
  (define (get-left p) (if (pair? p) (car p) p))
  (define (get-right p) (if (pair? p) (cdr p) #f))
  (define left-ps (filter pict? (map get-left ps)))
  (define right-ps (filter pict? (map get-right ps)))
  (cond [(null? left-ps) (cons #f (apply vr-append sep right-ps))]
        [(null? right-ps) (apply vl-append sep left-ps)]
        [else (cons (apply vl-append sep left-ps) (apply vr-append sep right-ps))]))

(define (fix-floats p)
  (if (pair? p) (rt-superimpose (car p) (cdr p)) p))

(define (rendered-block-width p)
  (if (pair? p) (max (if (car p) (pict-width (car p)) 0) (pict-width (cdr p))) (pict-width p)))

;; ------------------------------------------------------------
;; Table Styles

(define (add-table-style style istyle)
  (match style
    [(s:style name props)
     (foldl add-table-style-prop (add-table-style name istyle) props)]
    ['centered
     (hash-set istyle 'block-halign 'center)]
    [_ (add-block-style style istyle)]))

(define (add-table-style-prop prop istyle)
  (match prop
    [(s:table-cells styless)
     (hash-set istyle 'table-cells styless)]
    [(s:table-columns styles)
     (hash-set istyle 'table-cols styles)]
    [_ (add-block-style-prop prop istyle)]))

(define (remove-table-styles istyle)
  (remove-block-styles (hash-remove* istyle '(table-cells table-cols))))

(define (apply-table-styles istyle p)
  (apply-block-styles istyle p))

;; ------------------------------------------------------------
;; Table Cell Styles

;; Table cell style keys:
;; - 'cell-border : (Listof (U 'all 'left 'right 'top 'bottom))
;; - 'cell-halign : (U 'left 'right 'center)
;; - 'cell-valign : (U 'top 'bottom 'vcenter 'baseline) -- currently ignored
;; - 'cell-bgcolor : (U color% String)

(define (add-table-cell-style style istyle)
  (match style
    [(s:style name props)
     (foldl add-table-cell-style-prop (add-table-cell-style name istyle) props)]
    [_ (add-block-style style istyle)]))

(define (add-table-cell-style-prop prop istyle)
  (match prop
    [(or 'left 'right 'center)
     (hash-set istyle 'cell-halign prop)]
    [(or 'top 'bottom 'vcenter 'baseline)
     (hash-set istyle 'cell-valign prop)]
    ['border        (hash-cons istyle 'cell-border 'all)]
    ['left-border   (hash-cons istyle 'cell-border 'left)]
    ['right-border  (hash-cons istyle 'cell-border 'right)]
    ['top-border    (hash-cons istyle 'cell-border 'top)]
    ['bottom-border (hash-cons istyle 'cell-border 'bottom)]
    [(s:background-color-property color)
     (hash-set 'istyle 'cell-bgcolor (to-color color))]
    [_ (add-block-style-prop prop istyle)]))

(define (remove-table-cell-styles istyle)
  (hash-remove* istyle '(cell-halign cell-valign cell-border cell-bgcolor)))

(define (apply-table-cell-styles istyle width rb)
  (let* ([p (if (pair? rb) (or (car rb) (blank)) rb)]
         [p (let ([dwidth (- width (pict-width p))])
              (case (hash-ref istyle 'cell-halign 'left)
                [(left) (inset p 0 0 dwidth 0)]
                [(right) (inset p dwidth 0 0 0)]
                [(center) (inset p (/ dwidth 2) 0 (/ dwidth 2) 0)]))]
         [p (if (pair? rb) (rt-superimpose p (cdr rb)) p)]
         [p (cond [(hash-ref istyle 'cell-bgcolor #f)
                   => (lambda (c) (bg-colorize p c))]
                  [else p])]
         [p (cond [(hash-ref istyle 'cell-border #f)
                   => (lambda (borders) (add-borders p borders))]
                  [else p])])
    (apply-block-styles istyle p)))

;; ============================================================

;; A Flow is (Listof Block).
;; A Block is one of
;; - (itemization Style (Listof Flow))
;; - (paragraph Style Content)
;; - ... some other things ...

;; flow->pict : Flow IStyle -> Pict
(define (flow->pict blocks istyle)
  (fix-floats (render-flow blocks istyle)))

;; render-flow : Flow IStyle -> RenderedBlock
(define (render-flow blocks istyle)
  (append-blocks (get-block-sep istyle)
                 (for/list ([block (in-list blocks)])
                   (render-block block istyle))))

;; render-block : Block IStyle -> RenderedBlock
(define (render-block block istyle)
  (match block
    [(s:paragraph style content)
     (let* ([istyle (add-block-style style istyle)]
            [width (hash-ref istyle 'block-width)])
       (define p (content->pict content (remove-block-styles istyle) width))
       (apply-block-styles istyle p))]
    [(s:compound-paragraph style blocks)
     (let ([istyle (add-block-style style istyle)])
       (append-blocks (get-line-sep istyle)
                      (for/list ([block (in-list blocks)])
                        (render-block block istyle))))]
    [(s:nested-flow style flow)
     (render-flow flow (add-block-style style istyle))]
    [(s:itemization style flows)
     (define bullet (get-bullet istyle))
     (define bullet-width (+ (pict-width bullet) 10))
     (define sub-width (- (hash-ref istyle 'block-width +inf.0) bullet-width))
     (let* ([istyle (add-block-style style istyle)]
            [istyle (hash-set istyle 'block-width sub-width)])
       (append-blocks (get-line-sep istyle)
                      (for/list ([flow (in-list flows)])
                        (htl-append 10 bullet (flow->pict flow istyle)))))]
    [(s:table style blockss)
     (let ([istyle (hash-set istyle 'inset-to-width? #f)])
       (table->pict blockss (add-table-style style istyle)))]))

(define (table->pict cellss istyle)
  (define nrows (length cellss))
  (define ncols (length (car cellss)))
  (define col-styles (or (hash-ref istyle 'table-cols #f) (make-list ncols #f)))
  (define cell-styless (or (hash-ref istyle 'table-cells #f)
                           (make-list nrows (make-list ncols #f))))
  (define cell-istyle
    (hash-set* (remove-table-styles istyle) 'inset-to-width? #f 'block-width +inf.0))
  (define rendered-cellss ;; (Listof (Listof (U (cons Pict IStyle) #f)))
    (for/list ([cells (in-list cellss)]
               [cell-styles (in-list cell-styless)])
      (for/list ([cell (in-list cells)]
                 [cell-style (in-list cell-styles)]
                 [col-style (in-list col-styles)])
        (cond [(eq? cell 'cont) #f]
              [else (render-table-cell cell cell-style col-style cell-istyle)]))))
  (define col-widths0
    (let ([columns (transpose rendered-cellss)])
      (for/fold ([rcolwidths null] [leftovers (make-list nrows 0)]
                 #:result (reverse rcolwidths))
                ([col (in-list columns)]
                 [next-col (in-list (cdr (append columns (list (make-list nrows #t)))))])
        (define eff-cell-widths
          (for/list ([cell (in-list col)] [leftover leftovers])
            (+ leftover (if cell (rendered-block-width (car cell)) 0))))
        (define col-width
          (apply max 0 (for/list ([eff-cell-width (in-list eff-cell-widths)]
                                  [next-cell (in-list next-col)]
                                  #:when next-cell)
                         eff-cell-width)))
        (define next-leftovers
          (for/list ([eff-cell-width (in-list eff-cell-widths)])
            (max 0 (- eff-cell-width col-width))))
        (values (cons col-width rcolwidths) next-leftovers))))
  (define col-widths
    (cond [(hash-ref istyle 'inset-to-width? #f)
           (define width (hash-ref istyle 'block-width 0))
           (define dwidth (max 0 (- width (apply + col-widths0))))
           (map (lambda (w) (+ w (/ dwidth ncols))) col-widths0)]
          [else col-widths0]))
  (define (row->pict rendered-cells)
    (for/fold ([acc null] [extra-width 0]
               #:result (apply hbl-append 0 acc)) ;; FIXME: valign????
              ([cell (in-list (reverse rendered-cells))]
               [width (in-list (reverse col-widths))])
      (cond [(not cell)
             (values acc (+ width extra-width))]
            [else
             (define cell-pict
               (apply-table-cell-styles (cdr cell) (+ width extra-width) (car cell)))
             (values (cons cell-pict acc) 0)])))
  (apply-table-styles istyle (apply vl-append 0 (map row->pict rendered-cellss))))

;; render-table-cell : Block Style Style IStyle -> (cons RenderedBlock IStyle)
(define (render-table-cell block cell-style col-style istyle0)
  (define istyle (add-table-cell-style cell-style (add-table-cell-style col-style istyle0)))
  (cons (render-block block (remove-table-cell-styles istyle)) istyle))

(define (transpose xss)
  (cond [(andmap pair? xss) (cons (map car xss) (transpose (map cdr xss)))]
        [else null]))

(define (get-bullet istyle)
  (define text-size (hash-ref istyle 'text-size BASE-SIZE))
  (arrowhead (* 2/3 text-size) 0))

;; ------------------------------------------------------------
;; Content

;; A Content is one of
;; - String
;; - Symbol in '(mdash ndash ldquo lsquo rdquo rsquo larr rarr prime)
;; - (element Style Content)
;; - Pict or PictConvertible
;; - (Listof Content)

(define (content->pict content istyle width)
  (define fs1 (content->rfragments content istyle))
  (define fs2 (coalesce-rfragments fs1))
  (define lines (linebreak-fragments fs2 width))
  (apply vl-append (get-line-sep istyle) lines))

;; Linebreaking algorithm:
;;
;; 1. Split strings into atomic fragments. A string fragment is either
;; all whitespace or whitespace-free.
;;
;; 2. Coalesce unbreakable sequences of fragments. A sequence of
;; fragments can be broken only before or after whitespace
;; fragment. FIXME: could add more breaking options, but easy to add
;; whitespace, so why bother?
;;
;; 3. Pack lines using unbreakable sequences.

;; A Fragment is (fragment Pict Boolean), where a pict originating
;; from a string either contains no whitespace or only whitespace.
(struct fragment (pict ws?) #:prefab)

;; content->rfragments : Content IStyle -> (Listof Fragment), reversed
(define (content->rfragments content istyle)
  (define (loop content acc istyle)
    (match content
      [(? string?)
       (for/fold ([acc acc])
                 ([seg (in-list (string->segments (regexp-replace* "\n" content " ")))])
         (define p (base-content->pict seg istyle))
         (define ws? (and (regexp-match? #px"^\\s*$" seg)
                          (not (hash-ref istyle 'keep-whitespace? #f))))
         (cons (fragment p ws?) acc))]
      [(? symbol? s) (loop (content-symbol->string s) acc istyle)]
      [(? pict? p) (cons (fragment (base-content->pict p istyle) #f) acc)]
      [(s:element style content)
       (loop content acc (add-style style istyle))]
      [(s:delayed-element _ _ plain) (loop (plain) acc istyle)]
      [(s:part-relative-element _ _ plain) (loop (plain) acc istyle)]
      [(? pict-convertible?) (loop (pict-convert content) acc istyle)]
      [(? list? content)
       (for/fold ([acc acc]) ([part (in-list content)])
         (loop part acc istyle))]
      [_ (error 'content->rfragments "bad content: ~e" content)]))
  (loop content null istyle))

;; coalesce-rfragments : (Listof Fragment) -> (Listof Fragment), reversed
(define (coalesce-rfragments fs)
  (define (combine-inner ps) (apply hbl-append 0 ps))
  ;;(define (mark p) (frame p #:color "lightblue"))
  (define (outer-loop fs outer-acc)
    (match fs
      [(cons (and f (fragment p #t)) fs)
       (ws-loop fs (list p) outer-acc)]
      [(cons (and f (fragment p #f)) fs)
       (inner-loop fs (list p) outer-acc)]
      ['()
       outer-acc]))
  (define (ws-loop fs ws-acc outer-acc)
    (define (combine-ws ps) (mark (apply hbl-append 0 ps)))
    (define (mark p) p #;(frame p #:color "gray"))
    (match fs
      [(cons (fragment p #t) fs)
       (ws-loop fs (cons p ws-acc) outer-acc)]
      [_ (outer-loop fs (cons (fragment (combine-ws ws-acc) #t) outer-acc))]))
  (define (inner-loop fs inner-acc outer-acc)
    (define (combine-inner ps) (mark (apply hbl-append 0 ps)))
    (define (mark p) p #;(frame p #:color "lightblue"))
    (match fs
      [(cons (fragment p #f) fs)
       (inner-loop fs (cons p inner-acc) outer-acc)]
      [_ (outer-loop fs (cons (fragment (combine-inner inner-acc) #f) outer-acc))]))
  (outer-loop fs null))

(define (content-symbol->string sym)
  (case sym
    [(lsquo) "‘"] [(rsquo) "’"]
    [(ldquo) "“"] [(rdquo) "”"]
    [(mdash) "—"] [(ndash) "–"]
    [(prime) "′"]
    [(nbsp) " "] ;; non-breaking space
    [(rarr) "→"]
    [else (error 'content-symbol->string "unknown symbol: ~e" sym)]))

(define (base-content->pict content istyle)
  (define (~~> v . fs) (foldl (lambda (f v) (f v)) v fs))
  (define (finish p istyle text?)
    (~~> p
         (lambda (p)
           (cond [(hash-ref istyle 'color #f)
                  => (lambda (c) (colorize p c))]
                 [else p]))
         (lambda (p)
           (scale p (hash-ref istyle 'scale 1)))
         (lambda (p)
           (cond [(and text? (hash-ref istyle 'text-post #f))
                  => (lambda (posts)
                       (for/fold ([p p]) ([post (in-list (reverse posts))]) (post p)))]
                 [else p]))
         (lambda (p)
           (cond [(hash-ref istyle 'elem-post #f)
                  => (lambda (posts)
                       (for/fold ([p p]) ([post (in-list (reverse posts))]) (post p)))]
                 [else p]))
         (lambda (p)
           (cond [(hash-ref istyle 'bgcolor #f)
                  => (lambda (c) (bg-colorize p c))]
                 [else p]))))
  (match content
    [(? pict? p) (finish p istyle #f)]
    [(? string? str)
     (define ptstyle (append (hash-ref istyle 'text-mods null) (hash-ref istyle 'text-base)))
     (finish (text str ptstyle (hash-ref istyle 'text-size)) istyle #t)]))

;; linebreak-fragments : (Listof Fragment) PositiveReal -> (Listof Pict)
(define (linebreak-fragments fs width)
  (define (outer-loop fs outer-acc)
    (match fs
      ['() (reverse outer-acc)]
      [(cons (fragment _ #t) fs)
       (outer-loop fs outer-acc)]
      [fs ;; starts with non-ws fragment
       (inner-loop fs null 0 #f outer-acc)]))
  (define (inner-loop fs0 acc accw wsw outer-acc)
    (define (line)
      (let ([acc (if wsw (cdr acc) acc)])
        (apply hbl-append 0 (reverse acc))))
    (match fs0
      [(cons (fragment p ws?) fs)
       (define pw (pict-width p))
       (cond [(or (<= (+ accw pw) width) ;; line still has space
                  (and (null? acc) (not ws?))) ;; too long, but can't break
              (inner-loop fs (cons p acc) (+ accw pw) (if ws? pw #f) outer-acc)]
             [else
              (outer-loop fs0 (cons (line) outer-acc))])]
      ['()
       (outer-loop null (cons (line) outer-acc))]))
  (outer-loop fs null))

(define (string->segments s)
  ;; A Segment is a String that contains either all whitespace or no whitespace chars.
  (define ws-zones (regexp-match-positions* #px"\\s+" s))
  (let loop ([start 0] [ws-zones ws-zones])
    (cond [(null? ws-zones)
           (if (< start (string-length s)) (list (substring s start)) null)]
          [(< start (caar ws-zones))
           (cons (substring s start (caar ws-zones)) (loop (caar ws-zones) ws-zones))]
          [else
           (cons (substring s (caar ws-zones) (cdar ws-zones))
                 (loop (cdar ws-zones) (cdr ws-zones)))])))

(define (bg-colorize p c)
  (pin-under p 0 0 (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color c)))

(define (add-borders p borders)
  (define (has? sym) (or (memq sym borders) (memq 'all borders)))
  (define pw (pict-width p))
  (define ph (pict-height p))
  (let* ([p (if (has? 'left) (pin-over p 0 0 (vline 0 ph)) p)]
         [p (if (has? 'right) (pin-over p pw 0 (vline 0 ph)) p)]
         [p (if (has? 'top) (pin-over p 0 0 (hline pw 0)) p)]
         [p (if (has? 'bottom) (pin-over p 0 ph (hline pw 0)) p)])
    p))

;; ============================================================

(define (flow-pict #:style [style #f] . pre-flow)
  (define flow (s:decode-flow pre-flow))
  #;(debug-flow flow)
  (flow->pict flow (add-style style (current-istyle))))

#;
(define (debug-flow flow)
  (local-require racket/pretty)
  (define (simplify x)
    (match x
      [(s:paragraph style content)
       (s:paragraph (simplify-style style) (simplify content))]
      [(s:compound-paragraph style blocks)
       (s:compound-paragraph (simplify-style style) (simplify blocks))]
      [(s:nested-flow style flow)
       (s:nested-flow (simplify-style style) (simplify flow))]
      [(s:itemization style flows)
       (s:itemization (simplify-style style) (simplify flows))]
      [(s:table style blockss)
       (s:table (simplify-style style) (simplify blockss))]
      [(s:element style content)
       (s:element (simplify-style style) (simplify content))]
      [(s:delayed-element _ _ plain)
       (s:element 'spliced (simplify (plain)))]
      [(s:part-relative-element _ _ plain)
       (s:element 'spliced (simplify (plain)))]
      [(? list? xs) (map simplify xs)]
      [x x]))
  (define (simplify-style s)
    (define (keep-prop? x)
      (not (or (s:css-addition? x) (s:tex-addition? x))))
    (match s
      [(s:style s props)
       (s:style s (filter keep-prop? props))]
      [s s]))
  (parameterize ((pretty-print-columns 150))
    (pretty-print (simplify flow))))
