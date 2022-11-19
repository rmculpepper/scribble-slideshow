;; Copyright 2020-2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/class
         scribble/core
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

;; PreState = (lstate (Hasheqv Layer => {LPre, LPre, (cons IStyle NStyle)}))
(struct prestate (cur all style) #:prefab)

(define parts-renderer%
  (class object%
    (init-field config
                initial-default-layer)
    (super-new)

    (define/public (render-parts istyle parts)
      (define-values (st* tx) (handle-parts istyle parts (fresh-state)))
      (void (tx (fresh-post-state))))

    (define/public (render-part istyle part)
      (render-parts istyle (list part)))

    ;; ----------------------------------------

    ;; (Pre)State contains
    ;; - current LPre for each layer -- on current path
    ;; - "max" LPre for each layer -- over all paths, including alt paths
    ;; - istyle, nstyle for each layer

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
      (prestate (make-hasheqv) (make-hasheqv) (make-hasheqv)))

    (define/public (fork-state st)
      (match-define (prestate current-path-h all-paths-h styles-h) st)
      (prestate (hash-copy current-path-h) all-paths-h styles-h))

    (define/public (state-ref st lay)
      (match-define (prestate current-path-h all-paths-h _) st)
      (hash-ref current-path-h lay #f))

    (define/public (state-ref/all st lay)
      (match-define (prestate current-path-h all-paths-h _) st)
      (hash-ref all-paths-h lay #f))

    (define/public (state-styles! st lay [get #f])
      (match-define (prestate _ _ styles-h) st)
      (apply values
             (cond [get (hash-ref! styles-h lay (lambda () (call-with-values get list)))]
                   [else (hash-ref styles-h lay)])))

    (define/public (state-set! st lay lpre)
      (match-define (prestate current-path-h all-paths-h _) st)
      (hash-set! current-path-h lay lpre)
      (cond [(hash-ref all-paths-h lay #f)
             => (lambda (old-lpre)
                  (hash-set! all-paths-h lay (send lay max-pre lpre old-lpre)))]
            [else (hash-set! all-paths-h lay lpre)]))

    ;; ----------------------------------------

    ;; handle-parts : IStyle (Listof Part) PreState -> (values PreState PostTx)
    (define/public (handle-parts istyle ps st)
      (define-values (st* txs)
        (for/fold ([st st] [rtxs null] #:result (values st (reverse rtxs)))
                  ([p (in-list ps)])
          (define-values (st* tx) (handle-part istyle p st))
          (values st (cons tx rtxs))))
      (values st* (lambda (post) (for/fold ([post post]) ([f (in-list txs)]) (f post)))))

    ;; handle-part : IStyle Part PreState -> (values PreState PostTx)
    (define/public (handle-part istyle0 p in-st)
      (match-define (part _ _ title0 style _ blocks parts) p)
      (define-values (istyle nstyle) (add*-style style istyle0 #:kind 'slide))
      (case (hash-ref nstyle 'slide-ignore #f)
        [(ignore*)
         (values in-st (lambda (post) post))]
        [else
         (define slide-mode (hash-ref nstyle 'slide-mode #f))
         (define maker (wrap-maker istyle (or (hash-ref nstyle 'slide-maker #f) void)))
         (define title (if (hash-ref nstyle 'slide-no-title #f) #f title0))
         (define-values (final-st ptx)
           (handle-part* istyle nstyle title blocks parts (get-start-state slide-mode in-st)))
         (values (get-end-state slide-mode in-st final-st)
                 (case slide-mode
                   [(next)    (lambda (post) (maker) (ptx post))]
                   [(#f)      (lambda (post) (maker) (ptx (fresh-post-state)))]
                   [(alt)     (lambda (post) (maker) (ptx post) post)]
                   [(digress) (lambda (post) (maker) (ptx (fresh-post-state)) post)]))]))

    ;; handle-part* : __ -> (values PreState PostTx)
    (define/public (handle-part* istyle nstyle title blocks parts start-st)
      (define ignore-mode (hash-ref nstyle 'slide-ignore #f))
      (define bs-tx
        (cond [(or (eq? ignore-mode 'ignore)
                   (and (null? blocks) (pair? parts) (not (eq? ignore-mode 'no-ignore))))
               (lambda (post) post)]
              [else (handle-part-blocks istyle nstyle title blocks start-st)]))
      (define-values (final-st subs-tx) (handle-parts istyle parts start-st))
      (values final-st (lambda (post) (subs-tx (bs-tx post)))))

    ;; ----------------------------------------

    ;; PostState = (cons Pict/#f (Hasheqv Layer => (Listof Pict)))
    ;; PostTx = (PostState -> PostState), represents a renderer

    ;; Unlike (Pre)State, PostState is immutable. It represents
    ;; - (rendered?) title of previous slide
    ;; - rendered content of previous slide (for each layer)

    (define/public (fresh-post-state) (cons #f (hasheqv)))

    ;; handle-part-blocks : __ -> PostTx
    (define/public (handle-part-blocks istyle nstyle title blocks st)
      (define aspect (hash-ref nstyle 'aspect #f))
      (define layer=>blocks (split-blocks-by-layer blocks initial-default-layer))
      (define layer=>pict
        (for/fold ([layer=>pict (hasheq)])
                  ([(lay rblocks) layer=>blocks])
          (define-values (istyle* nstyle*)
            (state-styles! st lay (lambda () (send lay update-style istyle))))
          (define body-p (flow->pict (reverse rblocks) istyle*))
          (state-set! st lay (send lay update-pre (state-ref st lay) body-p))
          (hash-set layer=>pict lay body-p)))
      (define (render post)
        (match-define (cons in-title-p in-layer=>picts) post)
        (define title-p (get-title-pict istyle title in-title-p))
        (define layer=>picts
          (for/fold ([layer=>picts in-layer=>picts])
                    ([(lay body-p) (in-hash layer=>pict)])
            (hash-append layer=>picts lay (list body-p))))
        (emit-page title-p nstyle st layer=>picts)
        (cons title-p layer=>picts))
      render)

    (define/public (get-title-pict istyle title in-title-p)
      (match title
        [(list "..") in-title-p]
        [(or #f '()) #f]
        [else (let ([istyle (get-title-istyle istyle)])
                (content->pict title istyle +inf.0))]))

    (define/public (emit-page title-p nstyle st layer=>picts)
      (define aspect (hash-ref nstyle 'slide-aspect #f))
      (define sconf (make-config (and title-p #t) nstyle))
      (parameterize ((current-slide-config sconf))
        (define page (compose-page (send config fullpage) st layer=>picts))
        (send config slide/full title-p aspect page)))

    (define/public (compose-page base st layer=>picts)
      (for/fold ([base base])
                ([lay (in-list (sort (hash-keys layer=>picts) layer<?))])
        (define ps (hash-ref layer=>picts lay))
        (define lpre (state-ref/all st lay))
        (define-values (istyle nstyle) (state-styles! st lay))
        (send lay place ps lpre base istyle nstyle)))

    (define/public (make-config title? nstyle)
      (define aspect (hash-ref nstyle 'slide-aspect #f))
      (define layout (hash-ref nstyle 'slide-layout #f))
      (slide-config title? aspect layout))
    ))

;; current-slide-config used to communicate slide properties to default-layer%
(struct slide-config (title? aspect layout) #:prefab)
(define current-slide-config
  (make-parameter (slide-config #t 'fullscreen 'top)))

;; split-blocks-by-layer : (Listof Block) Layer -> Hash[Layer => (Listof Block), reversed]
(define (split-blocks-by-layer blocks default-layer)
  (define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))
  (let loop ([h (hasheq)] [blocks blocks] [layer default-layer])
    (for/fold ([h h]) ([b (in-list blocks)])
      (match b
        [(compound-paragraph (style 'set-layer (list (? layer? lay))) blocks)
         (loop h blocks lay)]
        [b (hash-cons h layer b)]))))

(define ((wrap-maker istyle maker))
  (parameterize ((current-istyle istyle)) (maker)))

;; ------------------------------------------------------------
;; Slide Styles

(struct make-slides-prop (mk)
  #:property prop:sp-style-prop
  (lambda (self istyle nstyle)
    (values istyle (hash-set nstyle 'slide-maker (make-slides-prop-mk self)))))

(define (get-title-istyle istyle0)
  (define-values (istyle nstyle) (add*-style 'slide-title istyle0))
  istyle)

;; ============================================================

(define (hash-append h k vs)
  (hash-set h k (append (hash-ref h k null) vs)))

(define (hash-update* h . kfs)
  (let loop ([h h] [kfs kfs])
    (match kfs
      [(list* k f kfs)
       (loop (hash-update h k f #f) kfs)]
      ['() h])))
