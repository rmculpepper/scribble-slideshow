;; Copyright 2019-2021 Ryan Culpepper.
;; Licensed under the Apache 2.0 license. See LICENSE.

#lang racket/base
(require racket/match
         racket/list
         racket/string
         racket/format
         (prefix-in s: scribble/core)
         pict pict/convert
         file/convertible
         "style.rkt"
         "linebreak.rkt")
(provide (all-defined-out))

;; ============================================================
;; Content

;; JustifyMode = (U 'greedy 'justify 'ragged)

;; content->pict : Content IStyle ExtReal HAlign -> Pict
(define (content->pict content istyle width [halign #f])
  (define lines (content->line-picts content istyle width))
  (define v*-append
    (case halign [(right) vr-append] [(center) vc-append] [else vl-append]))
  (define lines-p (apply v*-append (get-line-sep istyle) lines))
  (define debug? (memq 'linebreak (hash-ref istyle 'debug null)))
  (if debug? (inset/w width "lightblue" lines-p halign) lines-p))

;; content->line-picts : Content IStyle ExtReal -> (Listof Pict)
(define (content->line-picts content istyle width)
  (define mode
    (case (hash-ref istyle 'justify 'greedy)
      [(#t justify) 'justify]
      ;; [(ragged) 'ragged] ;; FIXME: ragged mode seems to be broken
      [else 'greedy]))
  (cond [(and (< width +inf.0) (memq mode '(justify ragged)))
         (kp-content->line-picts content istyle width mode)]
        [else (greedy-content->line-picts content istyle width)]))

;; inset/w : ExtReal Color Pict HAlign -> Pict
(define (inset/w w color p halign)
  (define dw (if (rational? w) (- w (pict-width p)) 0))
  (define-values (ldw rdw)
    (case halign
      [(center) (values (/ dw 2) (/ dw 2))]
      [(right) (values dw 0)]
      [else (values 0 dw)]))
  (define fp (frame #:segment 4 #:color color (inset p ldw 0 rdw 0)))
  (inset fp (- ldw) 0 (- rdw) 0))

;; ------------------------------------------------------------
;; Greedy linebreaking

;; FIXME: handle @nonbreaking{..}, 'no-break style

(define (greedy-content->line-picts content istyle width)
  (define is (content->items content istyle 'greedy))
  (greedy-linebreak-items is width))

;; greedy-linebreak-items : (Listof Item) PositiveReal -> (Listof Pict)
;; Note: this assumes adjacent non-ws boxes have already been coalesced.
(define (greedy-linebreak-items is width)
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
      [(cons (Box bp bw) is)
       (cond [(or (<= (+ accw wsw bw) width) ;; line still has space
                  (null? acc)   ;; too long, but nothing to break!
                  (not ws?))    ;; too long, but not at break point (after ws)
              (let ([acc (cons bp (append wsacc acc))] [accw (+ accw wsw bw)])
                (inner-loop is outer-acc acc accw null 0 #f))]
             [else
              (outer-loop is0 (cons (line) outer-acc))])]
      [(cons (Glue gp gw _ _) is)
       (inner-loop is outer-acc acc accw (cons gp wsacc) (+ wsw gw) #t)]
      [(cons (Penalty _ _ -inf.0 _) is)
       (outer-loop is (cons (line) outer-acc))]
      [(cons (Penalty pp pw ppenalty _) is)
       (match is  ;; lookahead
         [(cons (Box bp bw) _)
          (cond [(> (+ accw wsw bw) width)
                 (outer-loop is (cons (line (cons pp (append wsacc acc))) outer-acc))]
                [else (inner-loop is outer-acc acc accw wsacc wsw ws?)])]
         [_ (inner-loop is outer-acc acc accw wsacc wsw ws?)])]
      ['()
       (outer-loop '() (cons (line) outer-acc))]))
  (outer-loop is null))

;; ------------------------------------------------------------
;; Knuth-Plass linebreaking

;; kp-content->line-picts : Content IStyle Width JustifyMode -> (Listof Pict)
(define (kp-content->line-picts content istyle width mode)
  (define iv (list->vector (content->items content istyle mode)))
  (define breaks (get-line-breaks iv width #:p 10))
  (define debug? (memq 'linebreak (hash-ref istyle 'debug null)))
  (kp-get-lines width iv breaks debug?))

;; kp-get-lines : ExtReal ItemVector (Listof Line) -> (Listof Pict)
(define (kp-get-lines width iv lines [debug? #f])
  (for/list ([ln (in-list lines)])
    (kp-get-line width iv ln debug?)))

;; kp-get-line : ExtReal ItemVector Line -> Pict
(define (kp-get-line width iv ln [debug? #f])
  (match-define (line start end0 adjratio) ln)
  (define end (if (Penalty? (vector-ref iv end0)) (add1 end0) end0))
  (define picts
    (for/fold ([acc null] #:result (reverse acc))
              ([it (in-vector iv start end)] [index (in-naturals start)])
      (match it
        [(Penalty p _ _ _) (if (= (add1 index) end) (cons p acc) acc)]
        [(? Glue? g) (cons (scale-glue g adjratio) acc)]
        [(Box p _) (cons p acc)])))
  (let ([linep (apply hbl-append 0 picts)])
    (if debug? (kp-debug-line linep adjratio) linep)))

;; kp-debug-line : Pict Real -> Pict
(define (kp-debug-line linep adjratio)
  (define rstr (if (rational? adjratio) (~r #:precision 2 adjratio) (~a adjratio)))
  (pin-over linep (pict-width linep) 0 (colorize (text rstr null 10) "pink")))


;; ============================================================
;; Content to Items

;; content->items : Content IStyle JustifyMode -> (Listof Item)
(define (content->items content istyle mode)
  (define coalesce? (eq? mode 'greedy))
  (define ritems (content->reversed-items content istyle mode))
  (if coalesce? (coalesce/reverse-items ritems) (reverse ritems)))

;; content->reversed-items : Content IStyle JustifyMode -> (Listof Item)
(define (content->reversed-items content istyle mode)
  (define (loop content acc istyle)
    (match content
      [(? string?)
       (define wsmode (hash-ref istyle 'white-space #f))
       (for/fold ([acc acc]) ([seg (in-list (string->segments content wsmode))])
         (cond [(equal? seg "\n")
                ;; FIXME: depends on ...
                (append reversed-newline-items acc)]
               [(regexp-match? #rx"^[ \t]+$" seg)
                (define seg-pict (base-content->pict seg istyle))
                (case wsmode
                  [(pre nowrap)
                   ;; keep whitespace; cannot break line
                   (cons (make-box seg-pict) acc)]
                  [(pre-wrap)
                   ;; keep whitespace, can break line before/after
                   (list* allow-break (make-box seg-pict) allow-break acc)]
                  [(#f)
                   ;; FIXME: get stretch/shrink from istyle
                   (append (reverse (make-glue seg-pict mode)) acc)]
                  [else (error 'content->items "unhandled wsmode ~e" wsmode)])]
               [#t ;; FIXME: get hyphenation mode from istyle
                ;; TODO: support hyphenation dictionary
                (append (reverse (hyphenations seg istyle)) acc)]
               [else
                (cons (make-box (base-content->pict seg istyle)) acc)]))]
      [(? symbol? s) (loop (content-symbol->string s) acc istyle)]
      [(? pict? p) (cons (make-box (base-content->pict p istyle)) acc)]
      [(s:element 'newline '("\n")) (append reversed-newline-items acc)]
      [(s:element style content)
       (loop content acc (add*-content-style style istyle))]
      ;; multiarg-element -- ??
      ;; render-element -- ??
      [(? s:traverse-element? e)
       (loop (parameterize ((current-istyle istyle))
               (s:traverse-element-content e (current-resolve-info)))
             acc istyle)]
      [(? s:delayed-element? e)
       (cond [(current-resolve-info)
              => (lambda (ri) (loop (parameterize ((current-istyle istyle))
                                      (s:delayed-element-content e ri))
                                    acc istyle))]
             [else (loop ((s:delayed-element-plain e)) acc istyle)])]
      [(? s:part-relative-element? e)
       (cond [(current-resolve-info)
              => (lambda (ri) (loop (parameterize ((current-istyle istyle))
                                      (s:part-relative-element-content e ri))
                                    acc istyle))]
             [else (loop ((s:part-relative-element-plain e)) acc istyle)])]
      [(? pict-convertible?)
       (loop (parameterize ((current-istyle istyle))
               (pict-convert content))
             acc istyle)]
      [(? convertible?)
       (loop (parameterize ((current-istyle istyle))
               (or (convert content 'scribble-content #f)
                   (convert content 'text #f)
                   (format "~.s" content)))
             acc istyle)]
      [(? list? content)
       (for/fold ([acc acc]) ([part (in-list content)])
         (loop part acc istyle))]
      [_ (error 'content->rfragments "bad content: ~e" content)]))
  (append reversed-newline-items (loop content null istyle)))

(define (add*-content-style s istyle0)
  (define-values (istyle nstyle) (add*-style s istyle0))
  istyle)

;; hyphenations : String IStyle -> (Listof Item)
(define (hyphenations str istyle)
  (define (to-box str)  ;; String -> Item
    (make-box (base-content->pict str istyle)))
  ;; If there are any hard hyphens, ignore soft hyphens.
  (cond [(regexp-match? #rx"-" str)
         (define hard-hyphen-penalty (make-hyphen-penalty (blank)))
         (define parts (hard-hyphenate str))
         (add-between (map to-box parts) hard-hyphen-penalty)]
        [(regexp-match? #rx"\U00AD" str)
         (define soft-hyphen-penalty (make-hyphen-penalty (base-content->pict "-" istyle)))
         (define parts (soft-hyphenate str))
         (add-between (map to-box parts) soft-hyphen-penalty)]
        [else (list (to-box str))]))

(define (soft-hyphenate str)
  (define soft-hyphen-rx #px"[\U00AD]")
  (string-split str soft-hyphen-rx #:trim? #f))

(define (hard-hyphenate str)
  ;; hard hyphen is eligible only if 2 alpha chars before and after
  (define hard-hyphen-rx #px"(?<=[[:alpha:]]{2}-)(?=[[:alpha:]]{2})")
  (string-split str hard-hyphen-rx #:trim? #f))

;; A Segment is a String that contains either
;; - no whitespace characters
;; - one or more linear whitespace characters (space and tab)
;; - exactly one newline ("\n"); may be multiple chars in source, eg "\n\r"

;; string->segments : String WhitespaceMode -> (Listof String)
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

;; content-symbol->string : Symbol -> String
(define (content-symbol->string sym)
  (case sym
    [(lsquo) "‘"] [(rsquo) "’"]
    [(ldquo) "“"] [(rdquo) "”"]
    [(mdash) "—"] [(ndash) "–"]
    [(prime) "′"]
    [(nbsp) " "] ;; non-breaking space
    [(larr) "←"] [(rarr) "→"]
    [else (error 'content-symbol->string "unknown symbol: ~e" sym)]))

;; base-content->pict : Content IStyle -> Pict
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


;; ============================================================
;; Specialize Items to Picts

;; make-box : Pict -> Item
(define (make-box p)
  (Box p (pict-width p)))

;; make-glue : Pict JustifyMode -> (Listof Item)
(define (make-glue wsp mode)
  (define w (pict-width wsp))
  (case mode
    [(greedy justify)
     (list (Glue wsp w (* w STRETCH) (* w SHRINK)))]
    [(ragged)
     ;; FIXME: broken?!
     (list disallow-break
           (Glue blank/eol 0 (* w 3) 0)
           allow-break
           (Glue wsp w (* w -3) 0))]
    [else (error 'make-glue "bad mode: ~e" mode)]))

;; make-hyphen-penalty : Pict -> Item
(define (make-hyphen-penalty hyphen-pict)
  (define pw (pict-width hyphen-pict))
  (Penalty hyphen-pict pw (* HYPHEN-PENALTY pw) #t))

(define HYPHEN-PENALTY 10) ;; ??

;; {allow,disallow,force}-break : Item
(define allow-break (Penalty (blank) 0 0 #f))
(define disallow-break (Penalty (blank) 0 +inf.0 #f))
(define force-break (Penalty (blank) 0 -inf.0 #f))

;; blank/eol : Pict
;; Hack! This pict is recognized by get-line/kp and not scaled, to
;; avoid filling last line. FIXME: handle justify/align better
(define blank/eol (blank))

;; reversed-newline-items : (Listof Item)
(define reversed-newline-items
  ;; Don't allow break at Glue, only at final Penalty.  (I think this
  ;; only really matters if break forced by overfull line.)
  ;; FIXME: use +inf.0 instead?
  (reverse (list disallow-break (Glue blank/eol 0 +inf.0 0) force-break)))

;; scale-glue : Glue Real -> Pict
(define (scale-glue g adjratio)
  (match-define (Glue p w stretch shrink) g)
  (define targetw
    (cond [(= adjratio 0) w]
          [(> adjratio 0) (+ w (* adjratio stretch))]
          [(< adjratio 0) (+ w (* adjratio shrink))]))
  (cond [(zero? adjratio) p]
        [(eq? p blank/eol) p]
        [(zero? w) (blank targetw 0)]
        [else (scale p (/ targetw w) 1)]))

;; ------------------------------------------------------------

;; coalesce/reverse-items : (Listof Item) -> (Listof Item)
(define (coalesce/reverse-items is)
  (define (loop is acc)
    (match is
      [(cons (Box bp bw) is)
       (box-loop is acc (list bp) bw)]
      [(cons (Glue gp gw gstr gshr) is)
       (glue-loop is acc (list gp) gw gstr gshr)]
      [(cons it is)
       (loop is (cons it acc))]
      ['() acc]))
  (define (box-loop is acc ps w)
    (match is
      [(cons (Box bp bw) is)
       (box-loop is acc (cons bp ps) (+ w bw))]
      [_ (loop is (cons (Box (apply hbl-append 0 ps) w) acc))]))
  (define (glue-loop is acc ps w str shr)
    (match is
      ;; FIXME: assumes all glue has same stretch/shrink factor
      [(cons (Glue gp gw gstr gshr) is)
       (glue-loop is acc (cons gp ps) (+ w gw) (+ str gstr) (+ shr gshr))]
      [_ (loop is (cons (Glue (apply hbl-append 0 ps) w str shr) acc))]))
  (loop is null))

;; ------------------------------------------------------------

(struct late-content (proc)
  #:constructor-name make-late-content
  #:omit-define-syntaxes
  #:property prop:convertible
  (lambda (self request default)
    (define c ((late-content-proc self)))
    (cond [(eq? request 'scribble-content) c]
          [(convertible? c) (convert c request default)]
          [else default])))

(define-syntax-rule (late-content c ...)
  (make-late-content (lambda () c ...)))
