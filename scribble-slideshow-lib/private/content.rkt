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
         "style.rkt")
(provide (all-defined-out))

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
