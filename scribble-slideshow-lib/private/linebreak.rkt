;; Copyright 2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         (only-in racket/math sqr)
         racket/list
         racket/class)
(provide (all-defined-out))

;; References:
;; - Breaking paragraphs into lines, Donald Knuth and Michael Plass
;; - The errors of TEX, Donald Knuth (abbreviated TEOT below)

;; ------------------------------------------------------------

;; An Item is one of
;; - (Box Any Real)
;; - (Glue Any Real Real Real)
;; - (Penalty Any Real ExtendedReal Boolean)  -- flagged? means hyphen
(struct Item (value width) #:prefab)
(struct Box Item () #:prefab)
(struct Glue Item (stretch shrink) #:prefab)
(struct Penalty Item (penalty flagged?) #:prefab)

(define (Item-width* it)
  (match it [(? Penalty?) 0] [_ (Item-width it)]))
(define (Item-stretch it)
  (match it [(Glue _ _ stretch _) stretch] [_ 0]))
(define (Item-shrink it)
  (match it [(Glue _ _ _ shrink) shrink] [_ 0]))
(define (Item-penalty it)
  (match it [(Penalty _ _ penalty _) penalty] [_ 0]))
(define (Item-flagged? it)
  (match it [(Penalty _ _ _ flagged?) flagged?] [_ #f]))

(define (Item-stretch-fin it)
  (let ([y (Item-stretch it)]) (if (= y +inf.0) 0 y)))
(define (Item-stretch-inf it)
  (let ([y (Item-stretch it)]) (if (= y +inf.0) 1 0)))

(define STRETCH 1/2)        ;; default whitespace stretch factor
(define SHRINK 1/3)         ;; default whitespace shrink factor

(define ALPHA 3000)         ;; default demerit for consecutive hyphenated lines (p1163)
(define GAMMA 3000)         ;; default demerit for fitness-class differences (p1163)
(define HYPHEN-PENALTY 50)  ;; default penalty for hyphen (p1163)
(define LINE-PENALTY 1)     ;; default penalty for each line (TEOT p631)
(define P-TOLERANCE 1.26)   ;; default tolerance (p1163)




;; ------------------------------------------------------------

(define (get-line-breaks items targetw #:p [p #f])
  (send kp-linebreaker get-line-breaks items targetw #:p p))

(define kp-linebreaker%
  (class object%
    (init-field [consecutive-hyphen-demerit ALPHA]
                [fitness-class-demerit GAMMA]
                [hyphen-penalty HYPHEN-PENALTY]
                [line-penalty LINE-PENALTY]
                [p-tolerance P-TOLERANCE]
                [ws-stretch STRETCH]
                [ws-shrink SHRINK])
    (super-new)

    (define/public (make-whitespace val w)
      (Glue val w (* w ws-stretch) (* w ws-shrink)))

    (define/public (make-hyphen val w)
      (Penalty val w hyphen-penalty #t))

    ;; --------------------

    (define/public (get-line-breaks items targetw #:p [p p-tolerance])
      (define para
        (new kp-para% (config this) (items items) (targetw targetw) (p-tolerance (or p p-tolerance))))
      (send para go))
    ))

(define kp-linebreaker (new kp-linebreaker%))

;; ------------------------------------------------------------

(define kp-para%
  (class object%
    (init-field config    ;; linebreaker% instance
                items     ;; (Vectorof Item)
                targetw   ;; PositiveReal
                p-tolerance)
    (define line-penalty (get-field line-penalty config))
    (define ALPHA (get-field consecutive-hyphen-demerit config))
    (define GAMMA (get-field fitness-class-demerit config))
    (define HYPHEN-PENALTY (get-field hyphen-penalty config))
    (super-new)

    ;; p1156: items must start with Box and end with (Penalty _ _ -inf.0)

    ;; "w" = width (finite)
    ;; "y" = stretchability (finite or +inf.0)
    ;; "z" = shrinkability (finite)

    (define len (vector-length items))      ;; "m" in BPL
    (define Sw (cumsum Item-width* items))  ;; (Vectorof Real), sum of previous w (p1157)
    (define Syf (cumsum Item-stretch-fin items)) ;; (Vectorof Real), sum of previous y (finite parts)
    (define Syi (cumsum Item-stretch-inf items)) ;; (Vectorof Nat), sum of previous y (# of +inf.0)
    (define Sz (cumsum Item-shrink items))  ;; (Vectorof Real), sum of previous z

    (define/public-final (get i) (vector-ref items i))
    (define/public-final (get-p i) (Item-penalty (get i)))
    (define/public-final (get-f i) (and (>= i 0) (Item-flagged? (get i))))

    (define/public-final (sum-w i j) (csumvec-diff Sw i j))
    (define/public-final (sum-y i j)
      (+ (csumvec-diff Syf i j)
         (if (zero? (csumvec-diff Syi i j)) 0 +inf.0)))
    (define/public-final (sum-z i j) (csumvec-diff Sz i j))

    ;; legal-break? : Index -> Boolean
    (define/public-final (legal-break? i) ;; p1125
      ;; A linebreak is legal only at
      ;; - a Glue item whose predecessor is a Box
      ;; - a Penalty item with a penalty < +inf.0
      (match (get i)
        [(Penalty _ _ penalty _) (< penalty +inf.0)]
        [(? Glue?) (and (> i 0) (Box? (get (sub1 i))))]
        [_ #f]))

    ;; forced-break? : Index -> Boolean
    (define/public-final (forced-break? i)
      (match (get i)
        [(Penalty _ _ -inf.0 _) #t]
        [_ #f]))

    ;; after : Index -> Index
    (define/public-final (after a)  ;; p1125, p1157
      (let loop ([i (add1 a)])
        (match (and (< i len) (get i))
          [(or #f (? Box?) (Penalty _ _ -inf.0 _)) i]
          [_ (loop (add1 i))])))

    ;; line-actual-length : Index Index -> Real
    (define/public (line-actual-length a b) ;; p1126
      (+ (sum-w a b)
         (let ([itemb (get b)])
           (if (Penalty? itemb) (Item-width itemb) 0))))

    ;; line-adjustment-ratio : Index Index -> ExtReal; p1126
    (define/public (line-adjustment-ratio a b)
      (define linew (line-actual-length a b))
      (define stretch (sum-y a b))
      (define shrink (sum-z a b))
      (cond [(= linew targetw) 0]
            [(< linew targetw)
             (if (> stretch 0) (/ (- targetw linew) stretch) +inf.0)]
            [(> linew targetw)
             (if (> shrink 0) (/ (- targetw linew) shrink) -inf.0)]))

    ;; line-badness : Index Index -> ExtReal
    (define/public (line-badness a b) ;; p1127, \beta
      (adjustment-ratio->badness (line-adjustment-ratio a b)))

    ;; adjustment-ratio->badness : ExtReal -> NNExtReal
    (define/public (adjustment-ratio->badness r) ;; p1127, \beta
      (cond [(< r -1) +inf.0]
            [else (* 100 (expt (abs r) 3))]))

    ;; line-demerits : Node Index ExtReal FitnessClass -> ExtReal
    (define/public (line-demerits anode b r bfitness) ;; p1128, \delta; TEOT p631
      (define a (node-position anode))
      (define aafter (node-after anode))
      (define badness (adjustment-ratio->badness r))
      (define penalty (get-p b))
      (define alpha (if (and (get-f a) (get-f b)) ALPHA 0))
      (define gamma (if (> (abs (- (node-fitness anode) bfitness)) 1) GAMMA 0))
      (cond [(>= penalty 0)
             (+ (sqr (+ line-penalty badness)) (sqr penalty) alpha gamma)]
            [(< -inf.0 penalty 0)
             (+ (sqr (+ line-penalty badness)) (- (sqr penalty)) alpha gamma)]
            [else ;; penalty = -inf.0
             (+ (sqr (+ line-penalty)) alpha gamma)]))

    ;; line-fitness-class : Index Index -> FitnessClass
    (define/public (line-fitness-class a b) ;; p1128
      (adjustment-ratio->class (line-adjustment-ratio a b)))

    ;; adjustment-ratio->class : ExtReal -> FitnessClass
    (define/public (adjustment-ratio->class r)
      (cond [(< r -0.5) 0] ;; tight
            [(< r 0.5) 1]  ;; normal
            [(< r 1.0) 2]  ;; loose
            [else 3]))     ;; very loose

    ;; ------------------------------------------------------------
    ;; Algorithm (p1148)

    ;; Badness tolerance parameter p ...
    ;; Looseness parameter q fixed at 0. (p1155)

    ;; A feasible break (p1148) is a point b for which the whole paragraph
    ;; can be broken into lines (at points [b1..bn=b] such that no line
    ;; has a badness exceeding p.

    (define/public (go)
      (cond [(= targetw +inf.0)
             ;; Only forced breaks; adjratio = 1
             (for/fold ([a 0] [acc null] #:result (reverse acc))
                       ([b (in-range 1 len)] #:when (forced-break? b))
               (values (after b) (cons (line a b 0) acc)))]
            [else
             (define result (go*))
             (and result (node->lines result))]))

    (define/public (go*)
      (define anode0 (initial-node))
      (define final-active
        (for/fold ([active (list anode0)])
                  ([b (in-range len)] #:when (legal-break? b))
          ;; (eprintf "b = ~s, ~e, active(~s)\n" b (get b) (length active))
          (update-active-breakpoints active b)))
      (and (pair? final-active) (argmin node-totdemerits final-active)))

    (define/private (update-active-breakpoints active b)
      ;; body of main loop
      (define-values (passive feasible)
        (for/fold ([passive null] [feasible null])
                  ([anode (in-list active)])
          (define r (line-adjustment-ratio (node-after anode) b))
          (values (cond [(or (< r -1) (forced-break? b))
                         (cons anode passive)]
                        [else passive])
                  (cond [(and (>= r -1) (< r p-tolerance))
                         (cons (make-break anode b r) feasible)]
                        [else feasible]))))
      (let* ([active* (if (pair? passive) (remq* passive active) active)]
             [active* (append (best-breaks feasible) active*)])
        (cond [(pair? active*) active*]
              [else (update-active/accept-overfull active b)])))

    (define/private (update-active/accept-overfull active b)
      (define breaks
        (for/fold ([breaks null])
                  ([anode (in-list active)])
          (define r (line-adjustment-ratio (node-after anode) b))
          (cons (make-break anode b (max -1 r)) breaks)))
      (best-breaks breaks))

    (define/private (make-break anode b r)
      (match-define (node a aafter aline ar afitness atotdemerits _) anode)
      (define bfitness (adjustment-ratio->class r))
      (define bdemerits (line-demerits anode b r bfitness))
      (node b (after b) (add1 aline) r bfitness (+ atotdemerits bdemerits) anode))

    (define/private (best-breaks breaks)
      ;; (p1159) Select at most one break per fitness, since we fix q=0.
      (for/fold ([keep null])
                ([fitness (in-range 0 4)])
        (for/fold ([best #f]
                   #:result (if best (cons best keep) keep))
                  ([bnode (in-list breaks)]
                   #:when (= fitness (node-fitness bnode)))
          (if (or (not best) (< (node-totdemerits bnode) (node-totdemerits best))) bnode best))))

    ))

;; p1158
(struct node
  (position     ;; Nat/-1 -- index of breakpoint (-1 means start of paragraph)
   after        ;; Nat -- after(position) = start of next line
   line         ;; Nat -- number of line ending here
   adjratio     ;; Real -- adjustment ratio
   fitness      ;; {0,1,2,3} -- fitness class of line ending here
   totdemerits  ;; Real -- total demerits up to this breakpoint
   previous     ;; Node/#f -- link to previous breakpoint
   ) #:prefab)

(define (initial-node)
  (node -1 0 0 1 1 0 #f))

(define (node<? x y)
  (let ([xline (node-line x)] [yline (node-line y)])
    (or (< xline yline) (and (= xline yline) (< (node-fitness x) (node-fitness y))))))

(define (cumsum f argv)
  (define len (vector-length argv))
  (define resv (make-vector len #f))
  (for/fold ([sum 0]) ([i (in-naturals)] [e (in-vector argv)])
    (vector-set! resv i sum)
    (+ sum (f e)))
  resv)

(define (csumvec-diff csvec i j)
  (- (vector-ref csvec j) (vector-ref csvec i)))

(struct line (start end adjratio) #:prefab)

(define (node->lines bn)
  (let loop ([bn bn] [acc null])
    (define an (node-previous bn))
    (cond [an (let ([ln (line (node-after an) (node-position bn) (node-adjratio bn))])
                (loop an (cons ln acc)))]
          [else acc])))
