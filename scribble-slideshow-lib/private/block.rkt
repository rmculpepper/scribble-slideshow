;; Copyright 2019-2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         pict pict/convert
         "style.rkt"
         "scribble.rkt"
         "content.rkt")
(provide (all-defined-out))

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
  (define-values (istyle* props*) (add-block-style* s istyle))
  istyle*)

(define (add-block-style* s istyle)
  (add-style* s istyle
              #:ignore-names '(;; paragraph
                               author pretitle wraps
                               ;; table
                               boxed centered block
                               ;; itemization
                               compact ordered
                               ;; nested-flow
                               inset code-inset vertical-inset
                               ;; compound-paragraph (none)
                               )
              #:ignore-props '(;; paragraph
                               omitable div never-indents
                               ;; table
                               aux never-indents
                               ;; itemization
                               never-indents
                               ;; nested-flow
                               command multicommand never-indents decorative pretitle
                               ;; compound-paragraph
                               command never-indents
                               )))


(define (add-block-style s istyle)
  (match s
    [(s:style name props)
     (foldl add-block-style-prop (add-block-style name istyle) props)]
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
     (define compact? #f)
     (let ([istyle (add-block-style style istyle)])
       (append-blocks (if compact? (get-line-sep istyle) (get-block-sep istyle))
                      (for/list ([block (in-list blocks)])
                        (render-block block istyle))))]
    [(s:nested-flow style flow)
     (render-flow flow (add-block-style style istyle))]
    [(s:itemization style flows)
     (define compact? (eq? (s:style-name style) 'compact))
     (define ordered? (eq? (s:style-name style) 'ordered))
     (let ([istyle (add-block-style style istyle)])
       (render-itemization compact? ordered? flows istyle))]
    [(s:table style blockss)
     (let ([istyle (hash-set istyle 'inset-to-width? #f)])
       (table->pict blockss (add-table-style style istyle)))]
    [(? s:traverse-block? block)
     (render-block (s:traverse-block-block block (current-resolve-info)) istyle)]
    [(? s:delayed-block? block)
     (append-blocks (get-block-sep istyle)
                    (for/list ([b (in-list (s:delayed-block-blocks block (current-resolve-info)))])
                      (render-block b istyle)))]
    ))

(define (render-itemization compact? ordered? flows istyle)
  (define bullets (for/list ([index (in-naturals 1)] [flow (in-list flows)])
                    (cond [ordered? (base-content->pict (format "~s." index) istyle)]
                          [else (get-bullet istyle)])))
  (define bullet-w (apply max 0 (map pict-width bullets)))
  (define bullet-sep (get-bullet-sep istyle)) ;; FIXME!
  (define sub-width (- (hash-ref istyle 'block-width +inf.0) bullet-w bullet-sep))
  (let ([istyle (hash-set istyle 'block-width sub-width)])
    (append-blocks (if compact? (get-line-sep istyle) (get-block-sep istyle))
                   (for/list ([bullet (in-list bullets)] [flow (in-list flows)])
                     (htl-append bullet-sep
                                 (inset bullet (- bullet-w (pict-width bullet)) 0 0 0)
                                 (flow->pict flow istyle))))))

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

;; ----------------------------------------

(define (get-bullet istyle)
  (define text-size (hash-ref istyle 'text-size BASE-SIZE))
  (arrowhead (* 2/3 text-size) 0))

(define (get-bullet-sep istyle)
  (define text-size (hash-ref istyle 'text-size BASE-SIZE))
  (* 1/2 text-size))

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

(define (flow-pict #:style [style #f] #:resolve? [resolve? #t] . pre-flow)
  (define flow (s:decode-flow pre-flow))
  #;(debug-flow flow)
  (parameterize ((current-resolve-info (if resolve? (resolve-flow flow) #f)))
    (flow->pict flow (add-style style (current-istyle)))))

(define (resolve-flow flow)
  (get-resolve-info (list (s:part #f null #f (s:style #f null) null flow null))))
