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

;; flow-pict : PreFlow ... -> Pict
(define (flow-pict #:style [style #f] #:resolve? [resolve? #t] . pre-flow)
  (define flow (s:decode-flow pre-flow))
  #;(debug-flow flow)
  (parameterize ((current-resolve-info (if resolve? (resolve-flow flow) #f)))
    (define-values (istyle nstyle) (add*-style style (current-istyle)))
    (call/block-style istyle nstyle (lambda (istyle) (flow->pict flow istyle)))))

;; resolve-flow : Flow -> ResolveInfo
(define (resolve-flow flow)
  (get-resolve-info (list (s:part #f null #f (s:style #f null) null flow null))))

;; ============================================================
;; Flow

;; A Flow is (Listof Block)

;; flow->pict : Flow IStyle -> Pict
(define (flow->pict blocks istyle)
  (fix-floats (render-flow blocks istyle)))

;; render-flow : Flow IStyle -> RenderedBlock
(define (render-flow blocks istyle)
  (append-blocks (get-block-sep istyle)
                 (for/list ([block (in-list blocks)])
                   (render-block block istyle))))

;; ----------------------------------------

;; A RenderedBlock is one of
;; - Pict                       -- normal case
;; - (rblo (U #f Pict) Pict)    -- cdr is floated to the right
(struct rblo (left right) #:prefab)

;; rendered-block-width : RenderedBlock -> Real
(define (rendered-block-width p)
  (if (pair? p) (max (if (car p) (pict-width (car p)) 0) (pict-width (cdr p))) (pict-width p)))

;; fix-floats : RenderedBlock -> Pict
(define (fix-floats rb)
  (match rb
    [(rblo left right)
     (rt-superimpose (or left (blank)) right)]
    [(? pict? p) p]))

;; append-blocks : PositiveReal (Listof RenderedBlock) -> RenderedBlock
(define (append-blocks sep ps)
  (define (get-left rb) (match rb [(rblo l r) l] [(? pict? p) p]))
  (define (get-right rb) (match rb [(rblo l r) r] [_ #f]))
  (define left-ps (filter pict? (map get-left ps)))
  (define right-ps (filter pict? (map get-right ps)))
  (cond [(null? left-ps) (rblo #f (apply vr-append sep right-ps))]
        [(null? right-ps) (apply vl-append sep left-ps)]
        [else (rblo (apply vl-append sep left-ps) (apply vr-append sep right-ps))]))


;; ============================================================
;; Block

;; render-block : Block IStyle -> RenderedBlock
(define (render-block block istyle)
  (match block
    [(s:paragraph style content)
     (render-paragraph style content istyle)]
    [(s:compound-paragraph style blocks)
     (render-compound-paragraph style blocks istyle)]
    [(s:nested-flow style flow)
     (render-nested-flow style flow istyle)]
    [(s:itemization style flows)
     (render-itemization style flows istyle)]
    [(s:table style blockss)
     (render-table style blockss istyle)]
    [(? s:traverse-block? block)  ;; no style
     (let ([forced-block (s:traverse-block-block block (current-resolve-info))])
       (render-block forced-block istyle))]
    [(? s:delayed-block? block)   ;; no style
     (let ([forced-blocks (s:delayed-block-blocks block (current-resolve-info))])
       (render-flow forced-blocks istyle))]
    ))

;; ------------------------------------------------------------
;; Paragraph

;; render-paragraph : Style Content IStyle -> Pict
(define (render-paragraph style content istyle0)
  (define-values (istyle nstyle) (add*-block-style style istyle0 #:kind 'paragraph))
  (call/block-style istyle nstyle
    (lambda (istyle)
      (define width (hash-ref istyle 'block-width))
      (define halign (hash-ref istyle 'block-halign #f))
      (content->pict content istyle width halign))))

;; ------------------------------------------------------------
;; Compound-paragraph

;; render-compound-paragraph : Style Flow IStyle -> RenderedBlock
(define (render-compound-paragraph style blocks istyle0)
  (define-values (istyle nstyle) (add*-block-style style istyle0 #:kind 'compound-paragraph))
  (call/block-style/rblo istyle nstyle
    (lambda (istyle)
      (append-blocks (get-block-sep istyle)
                     (for/list ([block (in-list blocks)])
                       (render-block block istyle))))))

;; ------------------------------------------------------------
;; Nested-flow

;; render-nested-flow : Style Flow IStyle -> Pict
(define (render-nested-flow style flow istyle0)
  (define-values (istyle nstyle) (add*-block-style style istyle0 #:kind 'nested-flow))
  (call/block-style istyle nstyle (lambda (istyle) (flow->pict flow istyle))))

;; ------------------------------------------------------------
;; Itemization

;; render-itemization : Style Flows IStyle -> Pict
(define (render-itemization style flows istyle0)
  (define-values (istyle nstyle) (add*-block-style style istyle0 #:kind 'itemization))
  (call/block-style istyle nstyle (lambda (istyle) (render-itemization* istyle nstyle flows))))

(define (render-itemization* istyle nstyle flows)
  (define ordered? (eq? (hash-ref nstyle 'itemization-mode #f) 'ordered))
  (define bullets (for/list ([index (in-naturals 1)] [flow (in-list flows)])
                    (cond [ordered? (base-content->pict (format "~s." index) istyle)]
                          [else (get-bullet istyle)])))
  (define bullet-w (apply max 0 (map pict-width bullets)))
  (define bullet-sep (get-bullet-sep istyle))
  (define sub-width (- (hash-ref istyle 'block-width +inf.0) bullet-w bullet-sep))
  (let ([istyle (hash-set* istyle
                           'block-width sub-width
                           'itemize-level (add1 (hash-ref istyle 'itemize-level 0)))])
    (append-blocks (get-block-sep istyle)
                   (for/list ([bullet (in-list bullets)] [flow (in-list flows)])
                     (htl-append bullet-sep
                                 (inset bullet (- bullet-w (pict-width bullet)) 0 0 0)
                                 (flow->pict flow istyle))))))

;; get-bullet-sep : IStyle -> Real
(define (get-bullet-sep istyle)
  (define text-size (hash-ref istyle 'text-size BASE-SIZE))
  (* 1/2 text-size))

(define DEFAULT-BULLETS '(arrowhead disk circle))

;; get-bullet : IStyle -> Pict
;; FIXME: make all bullets have constant width (per text-size) ??
(define (get-bullet istyle)
  (define text-size (hash-ref istyle 'text-size BASE-SIZE))
  (define level (hash-ref istyle 'itemize-level 0))
  (define bullets (hash-ref istyle 'itemize-bullets DEFAULT-BULLETS))
  (define bullet (if (< level (length bullets)) (list-ref bullets level) (last bullets)))
  (case bullet
    [(arrowhead) (bullet:arrowhead text-size)]
    [(disk) (bullet:circle text-size #t)]
    [(circle) (bullet:circle text-size #f)]
    [else (bullet:circle text-size #f)]))

(define (bullet:arrowhead text-size)
  #;(arrowhead (* 2/3 text-size) 0)
  (define SIZE (* 1/2 text-size))
  (define LIFT (* 5/8 #;3/5 text-size))
  (lift-above-baseline (arrowhead SIZE 0) LIFT))
(define (bullet:circle text-size fill?)
  #;(disk (* 1/4 text-size))
  (define SIZE (* 1/3 text-size))
  (define LIFT (* 1/2 text-size))
  (lift-above-baseline (if fill? (disk SIZE) (circle SIZE)) LIFT))

;; FIXME: allow per-item styles (eg, override bullet, etc), but itemization does
;; not contain item structure; maybe define item*, recognize certain use of
;; compound-paragraph directly within itemization?

;; ------------------------------------------------------------
;; Table

;; render-table : Style (Listof (Listof Block)) IStyle -> Pict
(define (render-table style cellss istyle0)
  (define-values (istyle nstyle) (add*-block-style style istyle0 #:kind 'table))
  (let ([istyle (hash-set istyle 'inset-to-width? #f)]) ;; !!
    (call/block-style istyle nstyle (lambda (istyle) (render-table* istyle nstyle cellss)))))

;; render-table* : IStyle NStyle (Listof (Listof Block)) -> Pict
(define (render-table* istyle nstyle cellss)
  (define nrows (length cellss))
  (define ncols (length (car cellss)))
  (define col-styles
    (or (hash-ref nstyle 'table-col-styles #f)
        (make-list ncols #f)))
  (define cell-styless
    (or (hash-ref nstyle 'table-cell-styless #f)
        (make-list nrows (make-list ncols #f))))
  ;; FIXME: check length of col-styles, cell-styless
  (define cell-istyle (hash-set* istyle 'inset-to-width? #f 'block-width +inf.0))
  (define rendered-cellss ;; (Listof (Listof (U RenderedCell #f)))
    (for/list ([cells (in-list cellss)]
               [cell-styles (in-list cell-styless)])
      (for/list ([cell (in-list cells)]
                 [cell-style (in-list cell-styles)]
                 [col-style (in-list col-styles)])
        (cond [(eq? cell 'cont) #f]
              [else (render-table-cell cell cell-style col-style cell-istyle)]))))
  (define col-widths0 (calculate-column-widths rendered-cellss nrows))
  (define col-widths
    (cond [(hash-ref istyle 'inset-to-width? #f)
           (define width (hash-ref istyle 'block-width 0))
           (define dwidth (max 0 (- width (apply + col-widths0))))
           (map (lambda (w) (+ w (/ dwidth ncols))) col-widths0)]
          [else col-widths0]))
  (define row->pict (make-row->pict col-widths))
  (apply vl-append 0 (map row->pict rendered-cellss)))

;; calculate-column-widths : (Listof (Listof RenderedCell/#f)) Nat -> (Listof Real)
(define (calculate-column-widths rendered-cellss nrows)
  (let ([columns (transpose rendered-cellss)])
    (for/fold ([rcolwidths null]
               [leftovers (make-list nrows 0)]
               #:result (reverse rcolwidths))
              ([col (in-list columns)]
               [next-col (in-list (cdr (append columns (list (make-list nrows #t)))))])
      (define eff-cell-widths
        (for/list ([cell (in-list col)] [leftover leftovers])
          (+ leftover (if cell (rendered-block-width (rcell-rb cell)) 0))))
      (define col-width
        (apply max 0 (for/list ([eff-cell-width (in-list eff-cell-widths)]
                                [next-cell (in-list next-col)]
                                #:when next-cell)
                       eff-cell-width)))
      (define next-leftovers
        (for/list ([eff-cell-width (in-list eff-cell-widths)])
          (max 0 (- eff-cell-width col-width))))
      (values (cons col-width rcolwidths) next-leftovers))))

;; make-row->pict : (Listof Real) -> (Listof RenderedCell/#f) -> Pict
(define ((make-row->pict col-widths) rendered-cells)
  (for/fold ([acc null] [extra-width 0] #:result (apply hbl-append 0 acc)) ;; FIXME: valign?
            ([cell (in-list (reverse rendered-cells))]
             [width (in-list (reverse col-widths))])
    (match cell
      [#f
       (values acc (+ width extra-width))]
      [(rcell rb istyle nstyle)
       (define cp (apply-table-cell-styles rb (+ width extra-width) istyle nstyle))
       (values (cons cp acc) 0)])))

(define (transpose xss)
  (cond [(andmap pair? xss) (cons (map car xss) (transpose (map cdr xss)))]
        [else null]))

;; ----------------------------------------
;; Table Cell

;; RenderedCell = (rcell RenderedBlock IStyle NStyle)
(struct rcell (rb istyle nstyle) #:prefab)

;; render-table-cell : Block Style Style IStyle -> (cons RenderedBlock IStyle)
(define (render-table-cell block cell-style col-style istyle0)
  (define-values (istyle1 nstyle1)
    (add*-block-style col-style istyle0 #:kind 'table-cell))
  (define-values (istyle nstyle)
    (add*-block-style cell-style istyle1 nstyle1 #:kind 'table-cell))
  (rcell (render-block block istyle) istyle nstyle))

;; ------------------------------------------------------------
;; Table Cell Styles

;; apply-table-cell-styles : RenderedBlock Real IStyle NStyle -> Pict
(define (apply-table-cell-styles rb width istyle nstyle)
  (let* ([p (if (pair? rb) (or (car rb) (blank)) rb)]
         [p (let ([dwidth (- width (pict-width p))])
              (case (hash-ref nstyle 'cell-halign 'left)
                [(left) (inset p 0 0 dwidth 0)]
                [(right) (inset p dwidth 0 0 0)]
                [(center) (inset p (/ dwidth 2) 0 (/ dwidth 2) 0)]))]
         [p (if (pair? rb) (rt-superimpose p (cdr rb)) p)]
         [p (cond [(hash-ref nstyle 'bgcolor #f)
                   => (lambda (c) (bg-colorize p c))]
                  [else p])]
         [p (cond [(hash-ref nstyle 'cell-border #f)
                   => (lambda (borders) (add-borders p borders))]
                  [else p])])
    p))


;; ============================================================
;; Block Styles

;; add*-block-style : Style IStyle NStyle [#:kind Symbol] -> (values IStyle NStyle)
(define (add*-block-style s istyle [nstyle #hasheq()] #:kind [kind #f])
  (define-values (istyle1 nstyle1) (add*-style s istyle nstyle #:kind kind))
  (hash-move istyle1 nstyle1 '(bgcolor)))

;; call/block-style : IStyle NStyle (IStyle -> Pict) -> Pict
(define (call/block-style istyle nstyle proc)
  (match-define (list ml mt mr mb) (or (hash-ref nstyle 'block-margin #f) '(0 0 0 0)))
  (match-define (list pl pt pr pb) (or (hash-ref nstyle 'block-padding #f) '(0 0 0 0)))
  (define istyle* (istyle-adjust-block-width istyle (- 0 ml mr pl pr)))
  (define base-p (proc istyle*))
  (define base-p* (apply-base-block-styles base-p istyle* nstyle))
  (define padded-p (inset base-p pl pt pr pb))
  (define padded-p* (apply-padded-block-styles padded-p istyle* nstyle))
  (inset padded-p ml mt mr mb))

;; call/block-style/rblo : IStyle NStyle (IStyle -> Pict) -> RenderedBlock
(define (call/block-style/rblo istyle nstyle proc)
  (define p (call/block-style istyle nstyle proc))
  (case (and (hash-ref istyle 'block-halign #f))
    [(float-right) (cons #f p)]
    [else p]))

;; apply-base-block-styles : Pict IStyle NStyle -> Pict
(define (apply-base-block-styles p istyle nstyle)
  (cond [(hash-ref istyle 'inset-to-width? #f)
         (define dwidth (- (hash-ref istyle 'block-width) (pict-width p)))
         (case (hash-ref istyle 'block-halign 'left)
           [(left) (inset p 0 0 dwidth 0)]
           [(right) (inset p dwidth 0 0 0)]
           [(center) (inset p (/ dwidth 2) 0)]
           [(float-right) p])]
        [else p]))

;; apply-padded-block-styles : Pict IStyle NStyle -> Pict
(define (apply-padded-block-styles p istyle nstyle)
  (let* ([p (cond [(hash-ref nstyle 'bgcolor #f)
                   => (lambda (c) (bg-colorize p c))]
                  [else p])]
         [p (cond [(hash-ref nstyle 'block-border #f)
                   => (lambda (borders) (add-borders p borders))]
                  [else p])])
    p))

;; add-borders : Pict (Listof Symbol) -> Pict
(define (add-borders p borders)
  (define (has? sym) (or (memq sym borders) (memq 'all borders)))
  (define pw (pict-width p))
  (define ph (pict-height p))
  (let* ([p (if (has? 'left) (pin-over p 0 0 (vline 0 ph)) p)]
         [p (if (has? 'right) (pin-over p pw 0 (vline 0 ph)) p)]
         [p (if (has? 'top) (pin-over p 0 0 (hline pw 0)) p)]
         [p (if (has? 'bottom) (pin-over p 0 ph (hline pw 0)) p)])
    p))
