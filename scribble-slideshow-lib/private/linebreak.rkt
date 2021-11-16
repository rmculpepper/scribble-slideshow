#lang racket/base
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


(struct ivec
  (iv   ;; (Vectorof Item) -- the items
   Sw   ;; (Vectorof Real) -- sum of previous w (see p1157)
   Sy   ;; (Vectorof Real) -- sum of previous y
   Sz   ;; (Vectorof Real) -- sum of previous z
   ) #:transparent)

(define (cumsum f argv)
  (define len (vector-length argv))
  (define resv (make-vector len #f))
  (for/fold ([sum 0]) ([i (in-naturals)] [e (in-vector argv)])
    (vector-set! resv i sum)
    (+ sum (f e)))
  resv)

(define (csumvec-diff csvec i j)
  (- (vector-ref csvec j) (vector-ref csvec i)))

(define (make-ivec items)
  (define iv (list->vector items))
  (define len (vector-length iv))
  (define Sw (cumsum Item-width wv))
  (define Sy (cumsum Item-stretch iv))
  (define Sz (cumsum Item-shrink iv))
  (ivec iv Sw Sy Sz))

(define (ivec-ref ivec i) (vector-ref (ivec-iv ivec) i))
(define (ivec-w ivec i) (Item-width (vector-ref (ivec-iv ivec) i)))
(define (ivec-y ivec i) (Item-stretch (vector-ref (ivec-iv ivec) i)))
(define (ivec-z ivec i) (Item-shrink (vector-ref (ivec-iv ivec) i)))
(define (ivec-p ivec i) (Item-penalty (vector-ref (ivec-iv ivec) i)))
(define (ivec-f ivec i) (Item-flagged? (vector-ref (ivec-iv ivec) i)))

(define (ivec-sum-w ivec i j) (csumvec-diff (ivec-Sw ivec) i j))
(define (ivec-sum-y ivec i j) (csumvec-diff (ivec-Sy ivec) i j))
(define (ivec-sum-z ivec i j) (csumvec-diff (ivec-Sz ivec) i j))

(define (ivec-legal-break? ivec i) ;; p1125
  ;; A linebreak is legal only at
  ;; - a Glue item whose predecessor is a Box
  ;; - a Penalty item with a penalty < +inf.0
  (match (ivec-ref ivec i)
    [(Penalty _ _ penalty _) (< penalty +inf.0)]
    [(? Glue?) (and (> i 0) (Box? (ivec-item ivec (sub1 i))))]
    [_ #f]))

(define (ivec-after ivec a)  ;; p1125, p1157
  (define iv (ivec-iv ivec))
  (define len (vector-length iv))
  (let loop ([i (add1 a)])
    (match (and (< i len) (vector-ref iv i))
      [(or #f (? Box?) (Penalty _ _ -inf.0 _)) i]
      [_ (loop (add1 i))])))

(define (ivec-actual-length ivec a b) ;; p1126
  (+ (ivec-sum-w ivec a b)
     (let ([itemb (ivec-ref ivec b)])
       (if (Penalty? itemb) (Penalty-width itemb) 0))))

;; ivec-adjustment-ratio : ... -> (U Real #f); p1126
(define (ivec-adjustment-ratio ivec a b targetw)
  (define linew (ivec-actual-length ivec a b))
  (define stretch (ivec-sum-y ivec a b))
  (define shrink (ivec-sum-z ivec a b))
  (cond [(= linew targetw) 0]
        [(< linew targetw)
         (if (> stretch 0) (/ (- targetw linew) stretch) #f)]
        [(> linew targetw)
         (if (> shrink 0) (/ (- targetw linew) shrink) #f)]))

(define (ivec-badness ivec a b targetw) ;; p1127, \beta
  (adjustment-ratio->badness (ivec-adjustment-ratio ivec a b targetw)))

(define (adjustment-ratio->badness r) ;; p1127, \beta
  (cond [(or (not r) (< r -1)) +inf.0]
        [else (* 100 (expt (abs r) 3))]))

(define (ivec-demerits ivec a b targetw) ;; p1128, \delta
  ;; See also "The errors of TEX", p631.
  (define r (ivec-adjustment-ratio ivec a b targetw))
  (define badness (adjustment-ratio->badness r))
  (define penalty (ivec-p ivec b))
  (define LP 1) ;; linepenalty parameter
  (define alpha 0) ;; FIXME! Extra penalty for consecutive hyphenated lines.
  (cond [(>= penalty 0)
         (+ (sqr (+ LP badness penalty)) alpha)]
        [(< -inf.0 penalty 0)
         (+ (sqr (+ LP badness)) (- (sqr penalty)) alpha)]
        [else ;; penalty = -inf.0
         (+ (sqr (+ LP badness)) alpha)]))

;; Badness tolerance parameter p ...

;; A feasible break (p1148) is a point b for which the whole paragraph
;; can be broken into lines (at points [b1..bn=b] such that no line
;; has a badness exceeding p.

;; Looseness parameter q fixed at 0. (p1155)

;; ------------------------------------------------------------
;; Algorithm (p1148)

