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
  (content->pict/v1 content istyle width))

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

;; A Fragment is one of
;; - (fragment Pict WSMode), where a pict originating
;;   from a string either contains no whitespace or only whitespace,
;; - 'nl
(struct fragment (pict ws) #:prefab)
(struct fragment:string fragment (str istyle) #:prefab)

;; A WSMode is one of
;; - 'ws    -- soft whitespace: can break line, dropped at EOL
;; - #f     -- not whitespace: cannot break, cannot drop

;; content->rfragments : Content IStyle -> (Listof Fragment), reversed
(define (content->rfragments content istyle)
  (define (loop content acc istyle)
    (match content
      [(? string?)
       (define wsmode (hash-ref istyle 'white-space #f))
       (for/fold ([acc acc]) ([seg (in-list (string->segments content wsmode))])
         (cond [(equal? seg "\n")
                (cons 'nl acc)]
               [(regexp-match? #rx"^[ \t]+$" seg)
                (define seg-pict (base-content->pict seg istyle))
                (case wsmode
                  [(pre nowrap)
                   ;; keep whitespace; cannot break line
                   (cons (fragment seg-pict #f) acc)]
                  [(pre-wrap)
                   ;; keep whitespace, can break line before/after
                   (define nws (fragment (blank) 'ws))
                   (list* nws (fragment seg-pict #f) nws acc)]
                  [(#f) (cons (fragment seg-pict 'ws) acc)]
                  [else (error 'content->fragments "unhandled wsmode ~e" wsmode)])]
               [(regexp-match? #rx"[\u00AD]" seg)
                (cons (fragment:string (base-content->pict seg istyle) #f seg istyle) acc)]
               [else
                (cons (fragment (base-content->pict seg istyle) #f) acc)]))]
      [(? symbol? s) (loop (content-symbol->string s) acc istyle)]
      [(? pict? p) (cons (fragment (base-content->pict p istyle) #f) acc)]
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

;; ------------------------------------------------------------

(define (content->pict/v1 content istyle width)
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

;; FIXME: handle @|?-|, soft hyphen
;; FIXME: handle @nonbreaking{..}, 'no-break style

;; coalesce-rfragments : (Listof Fragment) -> (Listof Fragment), reversed
(define (coalesce-rfragments fs)
  (define (combine-inner ps) (apply hbl-append 0 ps))
  ;;(define (mark p) (frame p #:color "lightblue"))
  (define (outer-loop fs outer-acc)
    (match fs
      [(cons (and f (fragment p ws)) fs)
       (inner-loop ws fs (list p) outer-acc)]
      [(cons 'nl fs)
       (outer-loop fs (cons 'nl outer-acc))]
      ['() outer-acc]))
  (define (inner-loop ws fs inner-acc outer-acc)
    (match fs
      [(cons (fragment p (== ws)) fs)
       (inner-loop ws fs (cons p inner-acc) outer-acc)]
      [_ (outer-loop fs (cons (fragment (combine-inner inner-acc) ws) outer-acc))]))
  (outer-loop fs null))

;; linebreak-fragments : (Listof Fragment) PositiveReal -> (Listof Pict)
(define (linebreak-fragments fs width)
  (define (outer-loop fs outer-acc)
    (match fs
      ['() (reverse outer-acc)]
      [(cons (fragment _ 'ws) fs) ;; drop ws at start of line
       (outer-loop fs outer-acc)]
      [fs ;; starts with non-ws fragment
       (inner-loop fs null 0 #f outer-acc)]))
  (define (inner-loop fs0 acc accw wsw outer-acc)
    (define (line)
      (let ([acc (if wsw (cdr acc) acc)])
        (apply hbl-append 0 (reverse acc))))
    (match fs0
      [(cons 'nl fs)
       (outer-loop fs (cons (line) outer-acc))]
      [(cons (fragment p ws?) fs)
       (define pw (pict-width p))
       (cond [(or (<= (+ accw pw) width) ;; line still has space
                  (and (zero? accw) (not ws?))) ;; too long, but can't break
              (inner-loop fs (cons p acc) (+ accw pw) (if ws? pw #f) outer-acc)]
             [else
              (outer-loop fs0 (cons (line) outer-acc))])]
      ['()
       (outer-loop null (cons (line) outer-acc))]))
  (outer-loop fs null))

;; ------------------------------------------------------------