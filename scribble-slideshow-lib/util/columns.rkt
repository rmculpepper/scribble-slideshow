#lang racket/base
(require racket/match
         scribble/core
         scribble/decode
         "../private/style.rkt")
(provide columns
         column)

(struct col (style relw absw block)
  #:reflection-name 'column)

;; columns : Column ... -> Block
(define (columns #:style [s plain] . cols)
  (define total-relw (apply + (map col-relw cols)))
  (define total-absw (apply + (map col-absw cols)))
  (define ((col-width-f relw absw) istyle nstyle)
    (define outerw (hash-ref istyle 'table-width #f))
    (cond [(and (= (hash-ref istyle 'block-width 0) +inf.0) outerw)
           (define blockw (+ (* (if (zero? total-relw) 1 (/ relw total-relw))
                                (max 0 (- outerw absw)))
                             absw))
           (values (hash-set istyle 'block-width blockw) nstyle)]
          [else (values istyle nstyle)]))
  (define cols-prop
    (table-columns
     (for/list ([c (in-list cols)])
       (match-define (col s relw absw block) c)
       (style+ s (list (style-diffs (list (col-width-f relw absw))))))))
  (table (style+ s (list cols-prop)) (list (map col-block cols))))

(define (column #:style [s plain]
                #:width [w 1]
                #:valign [valign 'vcenter]
                #:halign [halign 'left]
                #:hmargin [hmargin #f]
                . pre-flow)
  (define-values (relw absw)
    (match w
      [(? real? w) (values w 0)]
      [(list '= (? real? w)) (values 0 w)]))
  (define hsp (and hmargin (style-diffs `((nset block-margin (,hmargin 0 ,hmargin 0))))))
  (define s* (style+ s (append (if hsp (list hsp) null) (list valign halign))))
  (define b (decode-compound-paragraph pre-flow))
  (col s* relw absw b))
