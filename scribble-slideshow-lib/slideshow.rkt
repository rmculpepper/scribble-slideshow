#lang racket/base
(require (only-in scribble/doclang [#%module-begin scribble-module-begin])
         "main.rkt"
         "private/slide.rkt")
(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out "main.rkt")
         (all-from-out "private/slide.rkt"))

;; ----------------------------------------
;; Module wrapper

(define-syntax-rule (module-begin id . body)
  (scribble-module-begin
   id post-process ()
   (module* main racket/base
     (require scribble-pict/slideshow
              (only-in (submod "..") [id id]))
     (scribble-slides* id))
   . body))

(define (post-process doc) doc)

;; ----------------------------------------
;; Reader

;; Adapted from scribble/manual/lang/reader.rkt
(module reader scribble/base/reader
  scribble-pict/slideshow
  #:wrapper1 (lambda (t) (cons 'doc (t))))
