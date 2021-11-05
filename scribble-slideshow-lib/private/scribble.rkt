#lang racket/base
(require racket/class
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
