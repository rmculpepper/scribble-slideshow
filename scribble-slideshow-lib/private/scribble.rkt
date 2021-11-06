#lang racket/base
(require racket/class
         racket/port
         scribble/base-render)
(provide (all-defined-out))

(define null-render%
  (class render%
    (inherit traverse
             collect
             resolve)
    (super-new (dest-dir #f))

    (define/override (install-file fn [content #f] #:private-name? [private-name? #t])
      (error 'null-render "cannot install file"))

    (define/override (install-extra-files ds)
      (void))

    (define/public (do-passes srcs [quiet? #t])
      (define dests (map (lambda (src) #f) srcs))
      (define fp (traverse srcs dests))
      (define ci (parameterize ((current-error-port (open-output-nowhere)))
                   ;; Suppress "WARNING: collected information ..." output.
                   (collect srcs dests fp)))
      (define ri (resolve srcs dests ci))
      ri)
    ))

(define the-null-render (new null-render%))

(define (get-resolve-info srcs)
  (send the-null-render do-passes srcs))

#;
(define (debug-flow flow)
  (local-require racket/pretty)
  (define (simplify x)
    (match x
      [(paragraph style content)
       (paragraph (simplify-style style) (simplify content))]
      [(compound-paragraph style blocks)
       (compound-paragraph (simplify-style style) (simplify blocks))]
      [(nested-flow style flow)
       (nested-flow (simplify-style style) (simplify flow))]
      [(itemization style flows)
       (itemization (simplify-style style) (simplify flows))]
      [(table style blockss)
       (table (simplify-style style) (simplify blockss))]
      [(element style content)
       (element (simplify-style style) (simplify content))]
      [(delayed-element _ _ plain)
       (element 'spliced (simplify (plain)))]
      [(part-relative-element _ _ plain)
       (element 'spliced (simplify (plain)))]
      [(? list? xs) (map simplify xs)]
      [x x]))
  (define (simplify-style s)
    (define (keep-prop? x)
      (not (or (css-addition? x) (tex-addition? x))))
    (match s
      [(style s props)
       (style s (filter keep-prop? props))]
      [s s]))
  (parameterize ((pretty-print-columns 150))
    (pretty-print (simplify flow))))
