;; This slideshow can be run using the `racket` command:
;;
;;   racket PATH/TO/demo.rkt
;;   racket -l scribble-slideshow/examples/demo
;;
;; or it can be run with the `slideshow` command:
;;
;;   slideshow PATH/TO/demo.rkt
;;
;; Using the slideshow command allows you to set options, render slides to PDF,
;; etc.

#lang scribble-slideshow
@(require (except-in pict table)
          pict/shadow
          scribble/core
          scribble/base
          scribble/manual
          scribble/html-properties
          scribble/latex-properties
          (for-label racket/base))

@(begin

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
               'slide-title-base '(bold . swiss))))


@title{Demo of @racketmodname[scribble-slideshow]}

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


@section{Some @racketmodname[scribble/manual] examples}

Styles are supported on an ad-hoc basis, but several manual forms are rendered
in something approximating their normal appearance.

@defproc[(foo [bar baz?]) quux/c]{

Returns the least @italic{quux} that is strictly more cromulent than
@racket[bar]. If none exists (for example, if @racket[bar] is @italic{perfectly}
cromulent), an exception is raised.}


@section{More Racket formatting}

To apply pict functions (such as @racket[frame]) to rendered Scribble, you can
use @racket[flow-pict] to turn a Scribble flow into a pict.

@frame[
@flow-pict[#:style 'roman]{
  @; -- Needs table!
  @racketblock[
  (define (map f xs)
    (if (pair? xs)
        (cons (f (car xs)) (map f (cdr xs)))
        '()))
  ]}]

@centered[
@flow-pict[#:style 'roman]{
  @racketgrammar*[
    #:literals (lambda)
    [X #, @elem{variable name}]
    [E X (lambda (X) E) (E E)]]
}
]


@section{Tables}

@tabular[#:sep @hspace[1] #:style 'centered
         (list (list "soup" "gazpacho")
               (list "soup" "tonjiru"))]

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{recipe}   @bold{vegetable})
               (list "caldo verde"   "kale")
               (list "kinpira gobō"  "burdock")
               (list "makizushi"     'cont)
               (list "This line ought to run into the second column." 'cont))]


@; You can include subsections (which get converted to slides) using Scribble's
@; standard `include-section` form. The section below illustrates staging
@; (roughly like slideshow's 'next and 'alts commands).

@include-section["demo-staging.scrbl"]


@section[#:style 'tall]{Layout}

This is a slide with @racket['tall] layout, even though it doesn't contain much.


@section[]      @; An empty section name generates a slide with no title.

That's the end!
