;; This example demonstrates the use of scribble-slideshow as a library.

;; This slideshow can be run using the `racket` command:
;;
;;   racket PATH/TO/demo2.scrbl
;;   racket -l scribble-slideshow/examples/demo2.scrbl
;;
;; or it can be run with the `slideshow` command:
;;
;;   slideshow PATH/TO/demo2.scrbl
;;
;; Using the slideshow command allows you to set options, render slides to PDF,
;; etc.

#lang at-exp racket/base
(require scribble-slideshow
         (prefix-in p: pict)            ;; avoid collisions; eg table
         (only-in slideshow/base slide) ;; avoid collisions; eg para, item, ...
         pict/shadow
         scribble/core
         scribble/base
         scribble/manual
         (for-label racket/base))

;; Here are some basic helper functions for constructing elements with text
;; or background colors, using Scribble's built-in style properties.

(define (blue . content)
  (apply elem #:style (style #f (list (color-property "blue"))) content))
(define (on-pink . content)
  (apply elem #:style (style #f (list (background-color-property "pink"))) content))

;; Here are some helpers that uses scribble-slideshow's `text-post-property`
;; to apply a post-transformation to the pict(s) rendered from the element.

(define (tx-elem f . content)
  (apply elem #:style (style #f (list (text-post-property f))) content))

(define ((shadow-tx rad dxy [color "purple"]) p)
  (shadow p rad dxy dxy #:shadow-color color))

;; Here we change some base style defaults.

(current-sp-style
 (hash-set* (current-sp-style)
            'text-base 'roman
            'slide-title-base '(bold . swiss)))

;; ============================================================

;; There are two main ways to use scribble-slideshow as a library:
;;   (1) `scribble-slides` and
;;   (2) `flow-pict`.
;; They can be used together. This module illustrates both.

;; ----------------------------------------

;; (1) Wrap Scribble code in a call to `scribble-slides`. It `decode`s the
;; Scribble document into a part (with sub-parts for sections) and converts
;; each part into a slide.

@scribble-slides{

@title{Demo of @racketmodname[scribble-slideshow] library}

This whole slide consists of a @italic{flow}, which contains multiple
@tt{paragraphs}. In turn, @tt{paragraphs} consist of
@bold{@elem[#:style 'sf]{elements}} and other stuff.

@para[#:style (style #f (list (background-color-property "yellow")))]{
  This is a @italic{paragraph}. It is written using @blue{Scribble's
  @litchar["@"]-exp reader}, which means that when I use @racket[para] and
  @racket[it] and picts, @on-pink{I do not have to break things
  @elem[#:style 'larger]{manually}}, like @racket[(para "This" (it "is") "a para")]
  --- I can write them @bold{``more naturally''}.
}

Benefits of @racketmodname[scribble-slideshow]:
@itemlist[

@item{so @tx-elem[(shadow-tx 6 2 "red")]{convenient} to write!}
@item{such @tx-elem[(shadow-tx 12 2 "blue")]{pretty} rendering!}

]


@section{More text formatting}

Subscripts and superscripts work, so you can talk about β@elem[#:style
'subscript]{v} and make fun of
L@elem[#:style 'superscript]{A}T@elem[#:style 'subscript]{E}X.

This is some centered text within a paragraph:
@centered{carpe diem}
and this the text that comes immediately after.

This is a whole new paragraph.@margin-note{With a margin note!}
}

;; ----------------------------------------

;; (2) Use `slide` directly and use `flow-pict` to turn Scribble "flow" into a
;; pict. Note that the titles are styled differently, because `scribble-slides`
;; uses its own styling and `slide` uses `(current-titlet)`.

(slide
 #:title "Some Racket manual examples"
 @flow-pict{

 Styles are supported on an ad-hoc basis, but several manual forms are rendered
 in something approximating their normal appearance.

 @defproc[(foo [bar baz?]) quux/c]{

 Returns the least @italic{quux} that is strictly more cromulent than
 @racket[bar]. If none exists (for example, if @racket[bar] is @italic{perfectly}
 cromulent), an exception is raised.}
 })

(slide
 #:title "More Racket formatting"

 @flow-pict{
   To apply pict functions (such as @racket[frame]) to rendered Scribble, you can
   use @racket[flow-pict] to turn a Scribble flow into a pict.
 }

 @p:frame[
 @flow-pict[#:style 'roman]{
   @racketblock[
   (define (map f xs)
     (if (pair? xs)
         (cons (f (car xs)) (map f (cdr xs)))
         '()))
   ]}]

 @flow-pict[#:style 'roman]{
   @racketgrammar*[
     #:literals (lambda)
     [X #, @elem{variable name}]
     [E X (lambda (X) E) (E E)]]
 })
