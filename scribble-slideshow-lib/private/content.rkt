;; Copyright 2019-2020 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/string
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         pict pict/convert
         "style.rkt")
(provide (all-defined-out))

(define (content->pict content istyle width)
  (content->pict/v2 content istyle width))


;; ============================================================
;; Content

;; A Content is one of
;; - String
;; - Symbol in '(mdash ndash ldquo lsquo rdquo rsquo larr rarr prime)
;; - (element Style Content)
;; - Pict or PictConvertible
;; - (Listof Content)

(define (content-symbol->string sym)
  (case sym
    [(lsquo) "‘"] [(rsquo) "’"]
    [(ldquo) "“"] [(rdquo) "”"]
    [(mdash) "—"] [(ndash) "–"]
    [(prime) "′"]
    [(nbsp) " "] ;; non-breaking space
    [(larr) "←"] [(rarr) "→"]
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

;; A Segment is a String that contains either
;; - no whitespace characters
;; - one or more linear whitespace characters (space and tab)
;; - exactly one newline ("\n"); may be multiple chars in source, eg "\n\r"
(define (string->segments s wsmode)
  (define ws-rx (if wsmode #rx"[ \t]+|\n\r?|\r" #px"\\s+"))
  (define (conv-ws s)
    (case wsmode
      [(pre pre-wrap) (case s [("\n" "\r" "\n\r") "\n"] [else s])]
      [(nowrap) (case s [("\n" "\r" "\n\r") " "] [else s])]
      [else #;(#f)  " "]))
  (define ws-zones (regexp-match-positions* #px"[ \t]+|\n\r?|\r" s))
  (let loop ([start 0] [ws-zones ws-zones])
    (cond [(null? ws-zones)
           (if (< start (string-length s)) (list (substring s start)) null)]
          [(< start (caar ws-zones))
           (cons (substring s start (caar ws-zones)) (loop (caar ws-zones) ws-zones))]
          [else
           (cons (conv-ws (substring s (caar ws-zones) (cdar ws-zones)))
                 (loop (cdar ws-zones) (cdr ws-zones)))])))

;; ------------------------------------------------------------
;; Specialize linebreaking support to picts

(define (make-box p)
  (Box p (pict-width p)))

(define STRETCH 1/2)
(define SHRINK 1/3)

(define (make-glue wsp)
  (let ([w (pict-width wsp)])
    (Glue ws-pict w (* w STRETCH) (* w SHRINK))))

(define (make-hyphen-penalty hyphen-pict)
  (define pw (pict-width hyphen-pict))
  (Penalty hyphen-pict pw (* HYPHEN-PENALTY pw) #t))

(define HYPHEN-PENALTY 10) ;; ??

(define break-ok (Penalty (blank) 0 0 #f))

;; ------------------------------------------------------------




;; content->items : Content IStyle -> (Listof Item)
(define (content->items content istyle)
  (coalesce/reverse-items (content->reversed-items content istyle)))

(define (content->reversed-items content istyle)
  (define (loop content acc istyle)
    (match content
      [(? string?)
       (define wsmode (hash-ref istyle 'white-space #f))
       (for/fold ([acc acc]) ([seg (in-list (string->segments content wsmode))])
         (cond [(equal? seg "\n")
                ;; FIXME: depends on ...
                (cons 'nl acc)]
               [(regexp-match? #rx"^[ \t]+$" seg)
                (define seg-pict (base-content->pict seg istyle))
                (case wsmode
                  [(pre nowrap)
                   ;; keep whitespace; cannot break line
                   (cons (make-box seg-pict) acc)]
                  [(pre-wrap)
                   ;; keep whitespace, can break line before/after
                   (list* break-ok (make-box seg-pict) break-ok acc)]
                  [(#f)
                   ;; FIXME: get stretch/shrink from istyle
                   (cons (make-glue seg-pict) acc)]
                  [else (error 'content->items "unhandled wsmode ~e" wsmode)])]
               [(regexp-match? #rx"[\u00AD]" seg)
                ;; FIXME: get hyphenation mode from istyle
                (append (reverse (hyphenations seg istyle)) acc)]
               [else
                (cons (make-box (base-content->pict seg istyle)) acc)]))]
      [(? symbol? s) (loop (content-symbol->string s) acc istyle)]
      [(? pict? p) (cons (make-box (base-content->pict p istyle)) acc)]
      [(s:element 'newline '("\n")) (cons 'nl acc)]
      [(s:element style content)
       (loop content acc (add-style style istyle))]
      ;; multiarg-element -- ??
      ;; render-element -- ??
      [(? s:traverse-element? e)
       (loop (s:traverse-element-content e (current-resolve-info)) acc istyle)]
      [(? s:delayed-element? e)
       (cond [(current-resolve-info)
              => (lambda (ri) (loop (s:delayed-element-content e ri) acc istyle))]
             [else (loop ((s:delayed-element-plain e)) acc istyle)])]
      [(? s:part-relative-element? e)
       (cond [(current-resolve-info)
              => (lambda (ri) (loop (s:part-relative-element-content e ri) acc istyle))]
             [else (loop ((s:part-relative-element-plain e)) acc istyle)])]
      [(? pict-convertible?) (loop (pict-convert content) acc istyle)]
      [(? list? content)
       (for/fold ([acc acc]) ([part (in-list content)])
         (loop part acc istyle))]
      [_ (error 'content->rfragments "bad content: ~e" content)]))
  (loop content null istyle))

(define (hyphenations str istyle)
  (define hyphen-penalty (make-hyphen-penalty (base-content->pict "-" istyle)))
  (define parts (map (lambda (s) (make-box (base-content->pict s istyle)))
                     (string-split str "\U00AD" #:trim? #f)))
  (add-between parts hyphen-penalty))

(define (coalesce/reverse-items is)
  (define (loop is acc)
    (match is
      [(cons (Box bps bw) is)
       (box-loop is acc bps bw)]
      [(cons (Glue gps gw gstr gshr) is)
       (glue-loop is acc gps gw gstr gshr)]
      [(cons it is)
       (loop is (cons it acc))]
      ['() acc]))
  (define (box-loop is acc ps w)
    (match is
      [(cons (Box bps bw) is)
       (box-loop is acc (append bps ps) (+ w bw))]
      [_ (loop is (cons (Box ps w) acc))]))
  (define (glue-loop is acc ps w str shr)
    (match is
      ;; FIXME: assumes all glue has same stretch/shrink factor
      [(cons (Glue gps gw gstr gshr) is)
       (glue-loop is acc (append gps ps) (+ w gw) (+ str gstr) (+ shr gshr))]
      [_ (loop is (cons (Glue ps w str shr) acc))]))
  (loop is null))

(define (inset/w w p)
  (define dw (if (rational? w) (- w (pict-width p)) 0))
  (inset (frame #:color "lightblue" (inset p 0 0 dw 0)) 0 0 (- dw) 0))

;; ------------------------------------------------------------
;; Greedy linebreaking

;; FIXME: handle @nonbreaking{..}, 'no-break style

(define (content->pict/v1 content istyle width)
  (define is (content->items content istyle))
  (define lines (linebreak-items/v1 is width))
  (inset/w width (apply vl-append (get-line-sep istyle) lines)))

;; linebreak-items/v1 : (Listof Item) PositiveReal -> (Listof Pict)
;; Note: this assumes adjacent non-ws boxes have already been coalesced.
(define (linebreak-items/v1 is width)
  (define (outer-loop is outer-acc)
    (match is
      ['() (reverse outer-acc)]
      [(cons (? Glue?) is) ;; drop glue at start of line
       (outer-loop is outer-acc)]
      [(cons (? Penalty?) is) ;; drop penalty at start of line
       (outer-loop is outer-acc)]
      [is ;; starts with non-ws item
       (inner-loop is outer-acc null 0 null 0 #f)]))
  (define (inner-loop is0 outer-acc acc accw wsacc wsw ws?)
    (define (line [acc acc]) (apply hbl-append 0 (reverse acc)))
    (match is0
      [(cons (Box bps bw) is)
       (cond [(or (<= (+ accw wsw bw) width) ;; line still has space
                  (null? acc)   ;; too long, but nothing to break!
                  (not ws?))    ;; too long, but not at break point (after ws)
              (let ([acc (append (reverse bps) wsacc acc)] [accw (+ accw wsw bw)])
                (inner-loop is outer-acc acc accw null 0 #f))]
             [else
              (outer-loop is0 (cons (line) outer-acc))])]
      [(cons (Glue gps gw _ _) is)
       (inner-loop is outer-acc acc accw (append (reverse gps) wsacc) (+ wsw gw) #t)]
      [(cons (Penalty pps pw ppenalty) is)
       (match is  ;; lookahead
         [(cons (Box bps bw) _)
          (cond [(> (+ accw wsw bw) width)
                 (outer-loop is (cons (line (append (reverse pps) wsacc acc)) outer-acc))]
                [else (inner-loop is outer-acc acc accw wsacc wsw ws?)])]
         [_ (inner-loop is outer-acc acc accw wsacc wsw ws?)])]
      [(cons 'nl is)
       (outer-loop is (cons (line) outer-acc))]
      ['()
       (outer-loop '() (cons (line) outer-acc))]))
  (outer-loop is null))

;; ------------------------------------------------------------
;; Justified

(define (content->pict/v2 content istyle width)
  (define iv (list->vector (content->items content istyle)))
  (define breaks (linebreaks/v2 iv istyle width))
  (define lines (get-lines/v2 width iv breaks))
  (inset/w width (apply vl-append (get-line-sep istyle) lines)))

(struct lbst
  (index    ;; Nat -- current index
   lastbr   ;; Nat -- previous break index
   linew    ;; Real -- width of current line to last non-ws
   wsw      ;; Real/#f -- width of ws at end of current line
   badness  ;; Real -- total badness of previous lines
   ) #:prefab)

(struct break
  (index    ;; Nat -- N means break line *before* item N
   lastbr   ;; Nat -- previous break
   linew    ;; Real -- width of current line to last non-ws item
   linebad  ;; Real -- badness of current line
   ) #:prefab)

;; A BreakTable is Hash[Nat => Break], stores least-badness break so far.

(define (linebreaks/v2 iv istyle width)
  (define btable (make-hasheqv))
  (define st0 (lbst 0 #f null 0))
  (define lbsts
    (for/fold ([sts (list st0)])
              ([it (in-vector iv)] [index (in-naturals)])
      (cut-lbsts width (append* (for/list ([st (in-list sts)]) (next-lbst index st it width))))))
  (define sorted-lbsts (sort lbsts < #:key lbst-badness))
  (reverse (lbst-breaks (car sorted-lbsts))))

(define (cut-lbsts width sts)
  (define N 16)
  (define sorted-sts (sort sts < #:key (predict-badness width)))
  (if (> (length sorted-sts) N) (take sorted-sts N) sorted-sts))

(define (next-lbst index st it width)
  (match-define (lbst inkw wsw breaks badness) st)
  (match it
    ['nl
     (list (lbst 0 #f (cons (add1 index) breaks)
                 (+ badness (line-badness width inkw #f))))]
    [(Penalty pps pw ppenalty)
     (list (lbst inkw wsw breaks badness)
           (lbst 0 #f (cons (add1 index) breaks)
                 (+ badness ppenalty (line-badness width (+ inkw (or wsw 0) pw)))))]
    [(Glue gps gw gstr gshr)
     (list (lbst inkw (+ (or wsw 0) gw) breaks badness))]
    [(Box bps bw)
     (define (continue) (lbst (+ inkw (or wsw 0) bw) #f breaks badness))
     (define (break) (lbst bw #f (cons index breaks) (+ badness (line-badness width inkw))))
     (cond [(zero? inkw) ;; can't break, no ink on current line yet
            (list (continue))]
           [(not wsw)  ;; can't break, not after ws
            (list (continue))]
           [else (list (continue) (break))])]))

(define ((predict-badness width) st)
  (match-define (lbst inkw wsw breaks badness) st)
  (+ badness (line-badness width inkw #f)))

(define (line-badness width inkw [under-bad? #t])
  (define over (max 0 (- inkw width)))
  (define under (if under-bad? (max 0 (- width inkw)) 0))
  (+ (expt over 3) (expt under 2)))

;; FIXME: calibrate wrt hyphen penalty

(define (get-lines/v2 width fv breaks)
  (let loop ([start 0] [breaks breaks])
    (match breaks
      ['() (list (get-line width fv start (vector-length fv) #t))]
      [(cons break breaks)
       (cons (get-line width fv start break #f)
             (loop break breaks))])))

(define (get-line width iv start end last?)
  (define (get-item index) (vector-ref iv index))
  (let ([short? (or last? (eq? 'nl (get-item (sub1 end))))]
        [start (let loop ([start start])
                 (if (and (< start end) (ws-item? (get-item start)))
                     (loop (add1 start))
                     start))]
        [end (let loop ([end end])
               (if (and (< start end) (ws-item? (get-item (sub1 end))))
                   (loop (sub1 end))
                   end))])
    (define-values (totw wsw)
      (for/fold ([totw 0] [wsw 0])
                ([it (in-vector iv start end)] [index (in-naturals)])
        (match it
          [(Penalty _ pw _)
           (if (= (add1 index) end)
               (values (+ totw pw) wsw)
               (values totw wsw))]
          [(Glue _ gw _ _)
           (values (+ totw gw) (+ wsw gw))]
          [(Box _ bw)
           (values (+ totw bw) wsw)])))
    (define wsscale0 ;; wsscale * wsw + inkw = width
      (let ([inkw (- totw wsw)])
        (cond [(zero? wsw) 1]
              [else (/ (- width inkw) wsw)])))
    (define wsscale (min (if short? 1 MAX-STRETCH) (max MIN-COMPRESS wsscale0)))
    (define picts
      (for/fold ([acc null] #:result (append* (reverse acc)))
                ([it (in-vector iv start end)] [index (in-naturals)])
        (match it
          [(Penalty pps _ _) (if (= (add1 index) end) (cons pps acc) acc)]
          [(Glue gps _ _ _) (cons (map (lambda (p) (scale p wsscale 1)) gps) acc)]
          [(Box bps _) (cons bps acc)])))
    (apply hbl-append 0 picts)))

(define (ws-item? it)
  (or (Glue? it) (eq? it 'nl)))

(define MIN-COMPRESS 0.6)
(define MAX-STRETCH 2.5)
