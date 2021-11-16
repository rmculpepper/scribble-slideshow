#lang racket/base
(require racket/match
         (only-in racket/math sqr)
         racket/list
         racket/class)
(provide (all-defined-out))

;; References:
;; - Breaking paragraphs into lines, Donald Knuth and Michael Plass
;; - The errors of TEX, Donald Knuth

;; ------------------------------------------------------------

;; An Item is one of
;; - (Box Any Real)
;; - (Glue Any Real Real Real)
;; - (Penalty Any Real ExtendedReal)
(struct Item (value width) #:prefab)
(struct Box Item () #:prefab)
(struct Glue Item (stretch shrink) #:prefab)
(struct Penalty Item (penalty flagged?) #:prefab)

(define (Item-stretch it)
  (match it [(Glue _ _ stretch _) stretch] [_ 0]))
(define (Item-shrink it)
  (match it [(Glue _ _ _ shrink) shrink] [_ 0]))
(define (Item-penalty it)
  (match it [(Penalty _ _ penalty _) penalty] [_ 0]))
(define (Item-flagged? it)
  (match it [(Penalty _ _ _ flagged?) flagged?] [_ #f]))

;; ------------------------------------------------------------

(define ALPHA 1000)
(define GAMMA 1000)

(define p-tolerance 5)

(define para%
  (class object%
    (init-field items     ;; (Vectorof Item)
                targetw   ;; PositiveReal
                [linepenalty 1]) ;; NNReal
    (super-new)

    ;; p1156: items must start with Box and end with (Penalty _ _ -inf.0)

    ;; "w" = width
    ;; "y" = stretchability
    ;; "z" = shrinkability

    (define len (vector-length items))      ;; "m" in BPL
    (define Sw (cumsum Item-width items))   ;; (Vectorof Real), sum of previous w (p1157)
    (define Sy (cumsum Item-stretch items)) ;; (Vectorof Real), sum of previous y
    (define Sz (cumsum Item-shrink items))  ;; (Vectorof Real), sum of previous z

    (define/public-final (get i) (vector-ref items i))
    (define/public-final (get-w i) (Item-width (get i)))
    (define/public-final (get-y i) (Item-stretch (get i)))
    (define/public-final (get-z i) (Item-shrink (get i)))
    (define/public-final (get-p i) (Item-penalty (get i)))
    (define/public-final (get-f i) (and (>= i 0) (Item-flagged? (get i))))

    (define/public-final (sum-w i j) (csumvec-diff Sw i j))
    (define/public-final (sum-y i j) (csumvec-diff Sy i j))
    (define/public-final (sum-z i j) (csumvec-diff Sz i j))

    (define/public-final (legal-break? i) ;; p1125
      ;; A linebreak is legal only at
      ;; - a Glue item whose predecessor is a Box
      ;; - a Penalty item with a penalty < +inf.0
      (match (get i)
        [(Penalty _ _ penalty _) (< penalty +inf.0)]
        [(? Glue?) (and (> i 0) (Box? (get (sub1 i))))]
        [_ #f]))

    (define/public-final (forced-break? i)
      (match (get i)
        [(Penalty _ _ -inf.0 _) #t]
        [_ #f]))

    (define/public-final (after a)  ;; p1125, p1157
      (let loop ([i (add1 a)])
        (match (and (< i len) (get i))
          [(or #f (? Box?) (Penalty _ _ -inf.0 _)) i]
          [_ (loop (add1 i))])))

    (define/public (line-actual-length a b) ;; p1126
      (+ (sum-w a b)
         (let ([itemb (get b)])
           (if (Penalty? itemb) (Item-width itemb) 0))))

    ;; line-adjustment-ratio : ... -> (U Real #f); p1126
    (define/public (line-adjustment-ratio a b)
      (define linew (line-actual-length a b))
      (define stretch (sum-y a b))
      (define shrink (sum-z a b))
      (cond [(= linew targetw) 0]
            [(< linew targetw)
             (if (> stretch 0) (/ (- targetw linew) stretch) +inf.0)]
            [(> linew targetw)
             (if (> shrink 0) (/ (- targetw linew) shrink) -inf.0)]))

    (define/public (line-badness a b) ;; p1127, \beta
      (adjustment-ratio->badness (line-adjustment-ratio a b)))

    (define/public (adjustment-ratio->badness r) ;; p1127, \beta
      (cond [(< r -1) +inf.0]
            [else (* 100 (expt (abs r) 3))]))

    (define/public (line-demerits anode b r bfitness) ;; p1128, \delta
      ;; See also "The errors of TEX", p631.
      (define a (node-position anode))
      (define aafter (node-after anode))
      (define badness (adjustment-ratio->badness r))
      (define penalty (get-p b))
      (define alpha (if (and (get-f a) (get-f b)) ALPHA 0))
      (define gamma (if (> (abs (- (node-fitness anode) bfitness)) 1) GAMMA 0))
      (cond [(>= penalty 0)
             (+ (sqr (+ linepenalty badness penalty)) alpha gamma)]
            [(< -inf.0 penalty 0)
             (+ (sqr (+ linepenalty badness)) (- (sqr penalty)) alpha gamma)]
            [else ;; penalty = -inf.0
             (+ (sqr (+ linepenalty badness)) alpha gamma)]))

    (define/public (line-fitness-class a b) ;; p1128
      (adjustment-ratio->class (line-adjustment-ratio a b)))

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
      (define result (go*))
      (and result
           (let loop ([n result] [acc null])
             (define next (node-previous n))
             (if next (loop next (cons (node-position n) acc)) acc))))

    (define/public (go*)
      (define anode0 (initial-node))
      (define final-active
        (for/fold ([active (list anode0)])
                  ([b (in-range len)] #:when (legal-break? b))
          (eprintf "b = ~s, active(~s) = ~e\n" b (length active) active)
          (update-active-breakpoints active b)))
      (and (pair? final-active) (argmin node-totdemerits final-active)))

    (define/public (initial-node)
      (node -1 0 0 1 0 #f))

    (define/private (update-active-breakpoints active b)
      ;; body of main loop
      (define-values (passive feasible)
        (for/fold ([passive null] [feasible null])
                  ([anode (in-list active)])
          (define r (line-adjustment-ratio (node-after anode) b))
          (values (cond [(or (< r -1) (forced-break? b))
                         (cons anode passive)]
                        [else passive])
                  (cond [(and (<= -1 r) (< r p-tolerance))
                         (cons (make-break anode b r) feasible)]
                        [else feasible]))))
      (let ([active (if (pair? passive) (remq* passive active) active)])
        (append (best-feasible-breaks feasible) active)))

    (define/private (make-break anode b r)
      (match-define (node a aafter aline afitness atotdemerits _) anode)
      (define bfitness (adjustment-ratio->class r))
      (define bdemerits (line-demerits anode b r bfitness))
      (node b (after b) (add1 aline) bfitness (+ atotdemerits bdemerits) anode))

    (define (best-feasible-breaks feasible)
      ;; (p1159) Select at most one break per fitness, since we fix q=0.
      (for/fold ([keep null])
                ([fitness (in-range 0 4)])
        (for/fold ([best #f]
                   #:result (if best (cons best keep) keep))
                  ([bnode (in-list feasible)]
                   #:when (= fitness (node-fitness bnode)))
          (if (or (not best) (< (node-totdemerits bnode) (node-totdemerits best))) bnode best))))

    ))

;; p1158
(struct node
  (position     ;; Nat/-1 -- index of breakpoint (-1 means start of paragraph)
   after        ;; Nat -- after(position) = start of next line
   line         ;; Nat -- number of line ending here
   fitness      ;; fitness class of line ending here
   totdemerits  ;; Real -- total demerits up to this breakpoint
   previous     ;; Node/#f -- link to previous breakpoint
   ) #:transparent)

;; FIXME: store adjustment-ratio in node!

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

;; ------------------------------------------------------------

(define sp (Glue " " 6 3 2))
(define words (for/list ([i (in-range 20)])
                (define len (modulo (sqr i) 6))
                (Box (make-string len #\x) (* len 6))))
(define items (append (add-between words sp) (list (Penalty "!" 0 -inf.0 #f))))
(define itemv (list->vector items))
;;itemv

(define para (new para% (items itemv) (targetw 50)))

(send para go)
