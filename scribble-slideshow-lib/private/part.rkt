;; Copyright 2020-2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/class
         racket/hash
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         scribble/decode
         (only-in pict inset)
         "style.rkt"
         "layer.rkt"
         "block.rkt"
         "content.rkt")
(provide (all-defined-out))

;; Each part is interpreted as a state transformer, something like this:
;;
;; (lambda (incoming-state)
;;   (define start-state (get-start-state incoming-state))
;;   (define-values (result intermediate-state) (create-immediate-content start-state))
;;   (define-values (more-results final-state) (create-subsections-content intermediate-state))
;;   (values (cons result more-results) (get-end-state incoming-state final-state)))
;;
;; Choices of behavior for (get-start-state incoming)
;; - incoming   -- use incoming state
;; - empty      -- clear to empty state
;;
;; Choices of behavior for (get-end-state incoming final)
;; - incoming   -- reset to own incoming state
;; - final      -- leave own final state for next
;;
;; Other possibilities (empty, start, intermediate, save-to-register&clear)
;; don't seem useful or necessary, or at least not yet worth the complexity.

;; The following section mode styles select combinations of the two behaviors:
;; - 'next = (incoming, final)
;; - 'alt  = (incoming, incoming)
;; - '#f   = (empty, final) -- the default
;; - 'digress = (empty, incoming)

(define parts-renderer%
  (class object%
    (init-field istyle
                initial-default-layer)
    (super-new)

    (define/public (render-parts parts)
      (define-values (st* tx) (handle-parts parts (fresh-state)))
      (void (tx (fresh-post-state))))

    (define/public (render-part part)
      (render-parts (list part)))

    ;; ----------------------------------------

    ;; (Pre)State contains
    ;; - current LPre for each layer -- on current path
    ;; - "max" LPre for each layer -- over all paths, including alt paths

    ;; (Pre)State is mutable; each part renderer holds a reference to its
    ;; pre-state, and by the time the renderer is called, the pre-state contains
    ;; information for all (future) paths.

    (define/public (get-start-state mode in-st)
      (case mode
        [(next) in-st]
        [(alt) (fork-state in-st)]
        [else #;(#f digress) (fresh-state)]))

    (define/public (get-end-state mode in-st final-st)
      (case mode
        [(alt digress) in-st]
        [else #;(next #f) final-st]))

    (define/public (fresh-state)
      (cons (make-hasheqv) (make-hasheqv)))

    (define/public (fork-state st)
      (match-define (cons current-path-st all-paths-st) st)
      (cons (hash-copy current-path-st) all-paths-st))

    (define/public (state-ref st lay)
      (match-define (cons current-path-h all-paths-h) st)
      (hash-ref current-path-h lay #f))

    (define/public (state-set st lay lpre)
      (match-define (cons current-path-h all-paths-h) st)
      (hash-set! current-path-h lay lpre)
      (cond [(hash-ref all-paths-h lay #f)
             => (lambda (old-lpre)
                  (hash-set! all-paths-h lay (send lay max-pre lpre old-lpre)))]
            [else (hash-set! all-paths-h lay lpre)]))

    ;; ----------------------------------------

    ;; handle-parts : (Listof Part) PreState -> (values PreState PostTx)
    (define/public (handle-parts ps st)
      (define-values (st* txs)
        (for/fold ([st st] [rtxs null]
                   #:result (values st (reverse rtxs)))
                  ([p (in-list ps)])
          (define-values (st* tx) (handle-part p st))
          (values st (cons tx rtxs))))
      (values st* (lambda (post) (for/fold ([post post]) ([f (in-list txs)]) (f post)))))

    ;; handle-part : Part PreState -> (values PreState PostTx)
    (define/public (handle-part p in-st)
      (match-define (part _ _ title0 style _ blocks parts) p)
      (define sstyles (add-slide-styles style (hash-ref istyle 'slide-styles (hasheq))))
      (case (hash-ref sstyles 'ignore #f)
        [(ignore*)
         (values in-st (lambda (post) post))]
        [else
         (define slide-mode (hash-ref sstyles 'mode #f))
         (define maker (or (hash-ref sstyles 'maker #f) void))
         (define title (if (hash-ref sstyles 'no-title #f) #f title0))
         (define-values (final-st ptx)
           (handle-part* sstyles title blocks parts (get-start-state slide-mode in-st)))
         (values (get-end-state slide-mode in-st final-st)
                 (case slide-mode
                   [(next)    (lambda (post) (maker) (ptx post))]
                   [(#f)      (lambda (post) (maker) (ptx (fresh-post-state)))]
                   [(alt)     (lambda (post) (maker) (ptx post) post)]
                   [(digress) (lambda (post) (maker) (ptx (fresh-post-state)) post)]))]))

    ;; handle-part* : __ -> (values PreState PostTx)
    (define/public (handle-part* sstyles title blocks parts start-st)
      (define bs-tx
        (case (hash-ref sstyles 'ignore #f)
          [(ignore) (lambda (post) post)]
          [else (handle-part-blocks sstyles title blocks start-st)]))
      (define-values (final-st subs-tx) (handle-parts parts start-st))
      (values final-st (lambda (post) (subs-tx (bs-tx post)))))

    ;; ----------------------------------------

    ;; PostState = (cons Pict/#f (Hasheqv Layer => (Listof Pict)))
    ;; PostTx = (PostState -> PostState), represents a renderer

    ;; Unlike (Pre)State, PostState is immutable. It represents
    ;; - (rendered?) title of previous slide
    ;; - rendered content of previous slide (for each layer)

    (define/public (fresh-post-state) (cons #f (hasheqv)))

    ;; handle-part-blocks : __ -> PostTx
    (define/public (handle-part-blocks sstyles title blocks st)
      (define aspect (hash-ref sstyles 'aspect #f))
      (define layer=>blocks (split-blocks-by-layer blocks initial-default-layer))
      (define layer=>pict
        (for/fold ([layer=>pict (hasheq)])
                  ([(lay rblocks) layer=>blocks])
          (define istyle* (send lay update-style istyle))
          (define body-p (flow->pict (reverse rblocks) istyle))
          (state-set st lay (send lay update-pre (state-ref st lay) body-p))
          (hash-set layer=>pict lay body-p)))
      (define (render post)
        (match-define (cons in-title-p in-layer=>picts) post)
        (define title-p (get-title-pict title in-title-p))
        (define layer=>picts
          (for/fold ([layer=>picts in-layer=>picts])
                    ([(lay body-p) (in-hash layer=>pict)])
            (hash-append layer=>picts lay (list body-p))))
        (emit-page title-p sstyles st layer=>picts)
        (cons title-p layer=>picts))
      render)

    (define/public (get-title-pict title in-title-p)
      (match title
        [(list "..") in-title-p]
        [(or #f '()) #f]
        [else (let ([istyle (get-title-istyle istyle)])
                (content->pict title istyle +inf.0))]))

    (define/public (compose-page base st layer=>picts)
      (for/fold ([base base])
                ([lay (in-list (sort (hash-keys layer=>picts) layer<?))])
        (define ps (hash-ref layer=>picts lay))
        (define lpre (state-ref st lay))
        (send lay place ps lpre base)))

    (abstract emit-page)
    ))

;; split-blocks-by-layer : (Listof Block) Layer -> Hash[Layer => (Listof Block), reversed]
(define (split-blocks-by-layer blocks default-layer)
  (define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
  (define (get-layer style)
    (for/or ([p (in-list (style-properties style))] #:when (layer? p)) p))
  (let loop ([h (hasheq)] [blocks blocks] [layer default-layer])
    (for/fold ([h h]) ([b (in-list blocks)])
      (match b
        [(compound-paragraph style blocks)
         (cond [(get-layer style)
                => (lambda (in-layer) (loop h blocks in-layer))]
               [else (hash-cons h layer b)])]
        [b (hash-cons h layer b)]))))

;; ------------------------------------------------------------

;; Slide Styles

;; A SlideStyles is a Hasheq with the following keys:
;; - layout : 'auto | 'center | 'top | 'tall
;; - aspect : 'widescreen | 'fullscreen | #f
;; - mode   : 'next | 'alt | #f | 'digress
;; - ignore : 'ignore | 'ignore*
;; - no-title : 'no-title | #f
;; - maker : Procedure

(struct make-slides-prop (mk))

(define (add-slide-styles s sstyles)
  (match s
    [(style name props)
     (for/fold ([sstyles sstyles])
               ([prop (in-list (cons name props))])
       (add-slide-style-prop prop sstyles))]))

(define (add-slide-style-prop prop sstyles)
  (match prop
    [(or 'auto 'center 'top 'tall) (hash-set sstyles 'layout prop)]
    [(or 'widescreen 'fullscreen) (hash-set sstyles 'aspect prop)]
    [(or 'next 'alt 'digress) (hash-set sstyles 'mode prop)]
    [(or 'ignore 'ignore*) (hash-set sstyles 'ignore prop)]
    [(or 'no-title) (hash-set sstyles 'no-title prop)]
    [(make-slides-prop mk) (hash-set sstyles 'maker mk)]
    ;; ----
    ;; standard scribble part styles seem irrelevant, ignore
    [_ sstyles]))

;; FIXME: move to single 'title-styles key?
(define (get-title-istyle istyle)
  (remove-slide-styles
   (remove-block-styles
    (hash-update* istyle
                  'color (lambda (v) (hash-ref istyle 'slide-title-color v))
                  'text-size (lambda (v) (hash-ref istyle 'slide-title-size v))
                  'text-base (lambda (v) (hash-ref istyle 'slide-title-base v))))))

(define (remove-slide-styles istyle)
  (define keys '(slide-title-color slide-title-size slide-title-base))
  (hash-remove* istyle keys))

;; ----------------------------------------

(define (hash-append h k vs)
  (hash-set h k (append (hash-ref h k null) vs)))

(define (hash-update* h . kfs)
  (let loop ([h h] [kfs kfs])
    (match kfs
      [(list* k f kfs)
       (loop (hash-update h k f #f) kfs)]
      ['() h])))
