;; This example demonstrates the use of "#lang scribble-slideshow", which uses
;; Scribble's at-notation and Scribble's module-body processing rules.

;; This slideshow can be run using the `racket` command:
;;
;;   racket PATH/TO/demo.scrbl
;;   racket -l scribble-slideshow/examples/demo.scrbl
;;
;; or it can be run with the `slideshow` command:
;;
;;   slideshow PATH/TO/demo.scrbl
;;
;; Using the slideshow command allows you to set options, render slides to PDF,
;; etc.

#lang scribble-slideshow
@(require (prefix-in p: pict)
          scribble-slideshow/util/columns
          (only-in slideshow [slide s:slide] [para s:para])
          pict/shadow
          scribble/core
          scribble/base
          scribble/manual
          (for-label racket/base))

@(define talk-style
   (style #f (list (style-diffs '((iset text-base roman justify #t debug (linebreak)))))))

@title[#:style talk-style]{Demo of @racketmodname[scribble-slideshow] language}

The @racketmodname[scribble-slideshow] language lets you write slideshow
presentations using Scribble notation and structures.

This slide consists of a Scribble @bold{flow}, which contains multiple
@bold{paragraphs}. Each @bold{paragraph} consists of @bold{content}:
@emph{elements and other stuff}.

@section{Styles}

@(begin
   ;; Defining a style for use with `elem`, etc.
   (define blue-style (style #f (list (color-property "blue"))))
   ;; Defining an `elem`-like wrapper function that applies a style.
   (define (hl . content)
     (apply elem #:style (style #f (list (background-color-property "yellow"))) content)))

The appearance of @bold{content} is determined by Scribble @bold{styles}:
@itemlist[

@item{@emph{Style properties} like @racket[color-property] and
@racket[background-color-property] @elem[#:style blue-style]{affect
individual aspects of @hl{text rendering} and @hl{document layout}}.}

@item{A @emph{style name} like @racket["RktErr"] bundles @elem[#:style
"RktErr"]{many style changes into a single package}.}

]



@;{

@section{blah}

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

@section[#:style 'ignore]{Ignored slide}

You should not see this.

@section[#:style 'ignore*]{Ignored* slide}

You should not see this slide...

@subsection{Child of ignored* slide}

... and you should not see this either.


@section{Two-column slides}

@columns[;; #:sep 50

@column[#:width 1/3]{
On the left, we have some text. It says a few things.
}

@column[#:width 2/3]{
On the right, we have more text.@(linebreak)
With an explicit linebreak.

It says @bold{more} things, things that aren't said on the left.

@racketgrammar*[
[things few-things more-things]
]}

]

@section{Two-column slides using layers}

@(begin
  (require ppict/zone (only-in racket/draw make-color))
  (define sd-t-red (style-diffs `((iset bgcolor (255 0 0 0.1)))))
  (define left-layer
    (slide-layer 'lt (coord-zone 0.0 1/4 0.38 1)
                 #:style (let ([post (lambda (p) (p:frame p #:color "red"))])
                           (style #f (list sd-t-red
                                           (style-diffs `((nset block-post (,post)))))))))
  (define right-layer
    (slide-layer 'lt (coord-zone 0.4 1/4 1.0 1)
                 #:style (let ([post (lambda (p)
                                        (define fp (shadow-frame p #:margin 5 #:shadow-descent 0
                                                                 #:shadow-alpha-factor 1/2))
                                        (p:refocus fp p))])
                           (style #f (list (style-diffs `((nset block-padding (5 5 5 5)
                                                                block-post (,post))))))))))

@compound*[#:layer left-layer]{
On the left, we have some text.
It says a @elem[#:style (style #f (list sd-t-red))]{few} things.
}

@compound*[#:layer right-layer]{
On the right, we have more text.@(linebreak)
With an explicit linebreak.

It says @bold{more} things, things that aren't said on the left.

@racketgrammar*[
[things few-things more-things]
]}

@section{More text formatting}

Subscripts and superscripts work, so you can talk about β@elem[#:style
'subscript]{v} and make fun of
L@elem[#:style 'superscript]{A}T@elem[#:style 'subscript]{E}X.

This is some centered text within a paragraph:
@centered{carpe diem}
and this the text that comes immediately after.

This is a whole new paragraph.@margin-note{With a margin note!}

@centered{This is a centered paragraph. Let's see what happens when we
have to combine line-breaking with block alignment!}

@section{Some @racketmodname[scribble/manual] examples}

Styles are supported on an ad-hoc basis, but several manual forms are rendered
in something approximating their normal appearance.

@defproc[(foo [bar baz?]) quux/c]{

Returns the least @italic{quux} that is strictly more crom@|?-|ulent than
@racket[bar]. If none exists (for example, if @racket[bar] is @italic{perfectly}
cromulent), an exception is raised.}


@section{More Racket formatting}

To apply pict functions (such as @racket[frame]) to ren@|?-|dered Scribble, you can
use @racket[flow-pict] to turn a Scribble flow into a pict.

@p:frame[
@flow-pict[#:style 'roman]{
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


@(define (mk-slides what) ;; Element -> (Listof pre-part?)
   @list{
     @section{Abstraction}

     It is possible to abstract over sections and their associated
     flow to create slides.

     @what is great!
     })

@(mk-slides "Abstraction")
@(mk-slides @tx-elem[(shadow-tx 5 2 "purple")]{Racket})


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

@(part/make-slides
  (lambda ()
   (s:slide
    @flow-pict{

    Use @racket[part/make-slides] to wrap a slide-emitting function as
    a Scribble @emph{part}. Any slides produced by the function are
    inserted at the location of the resulting part.

    })))

@section[#:style 'tall]{Layout}

This is a slide with @racket['tall] layout, even though it doesn't contain much.


@section[]      @; An empty section name generates a slide with no title.

That's the end!
}
