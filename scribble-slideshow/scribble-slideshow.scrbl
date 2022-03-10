#lang scribble/manual
@(require scribble/example
          (for-label racket/base scribble-slideshow
                     scribble/base scribble/core scribble/manual scribble/decode
                     (only-in pict pict?) (only-in slideshow slide)))

@(define (s-tech . pc)
   (apply tech #:doc '(lib "scribblings/scribble/scribble.scrbl") pc))

@(define (p-tech . pc)
   (apply tech #:doc '(lib "pict/scribblings/pict.scrbl") pc))

@(define repo "https://github.com/rmculpepper/scribble-slideshow/tree/master")

@(define the-eval (make-base-eval))
@(the-eval '(require scribble/base scribble/manual scribble-slideshow/pict))

@title[#:tag "scribble-slideshow"]{scribble-slideshow: Using Scribble to Make Slides}

@defmodule[scribble-slideshow #:lang]


@section{Introduction}

This library provides both a @emph{language} and a @emph{library} for writing
slideshows using Scribble notation. (To clarify, I don't mean just using the
@racketmodname[at-exp] reader with @racketmodname[slideshow]. I mean that the
slides are written using Scribble forms, evaluated to Scribble document
structures, and then rendered to slides and picts.) One benefit is that element
styles cooperate automatically with line-breaking.

The @racketmodname[scribble-slideshow] @emph{library} provides functions like
@racket[flow-pict] and @racket[scribble-slides] that convert Scribble
@s-tech{pre-flow} into @p-tech{picts} and emit slides from Scribble
@s-tech{parts}, respectively.

The @racketmodname[scribble-slideshow] @emph{language} allows users to write
Scribble documents that can automatically be run as slideshows. Like the
@racketmodname[scribble/base] and @racketmodname[scribble/manual] languages, the
contents of the module (minus requires, definitions, etc) are automatically
gathered and interpreted as a Scribble document. The language emits a @tt{main}
submodule that converts the Scribble document to slides.

See the @hyperlink[(format "~a/scribble-slideshow/example" repo)]{scribble-slideshow/examples}
directory for extended, runnable examples.


@section[#:tag "scribble-slide"]{Scribble to Slides}

@defproc[(scribble-slides [pre-part pre-part?] ...) void?]{

Decodes the @racket[pre-part]s to a Scribble @s-tech{part} (see @racket[decode])
and generates slides from its content and sub-parts. That is, there is an
additional slide generated for each section, sub-section, etc.

The style of each @racket[part] can be used to control a slide's layout and
staging. See the extended examples for more details.
}

@defproc[(part/make-slides [proc (-> void?)]) part?]{

Creates a @s-tech{part} wrapping a procedure. The procedure is run for
effect when the part is processed by @racket[scribble-slides] or its
equivalent; it should call @racket[slide] or similar functions to emit
slides for that part of the document.
}


@section[#:tag "scribble-pict"]{Scribble to Picts}

@defmodule[scribble-slideshow/pict]

The exports of @racketmodname[scribble-slideshow/pict] are also available from
@racketmodname[scribble-slideshow].

@defproc[(flow-pict [pre-flow pre-flow?] ...)
         pict?]{

Decodes the @racket[pre-flow]s into Scribble @s-tech{flow} (see
@racket[decode-flow]) and then converts it to a pict.

For example:
@codeblock[#:keep-lang-line? #f]|{
#lang at-exp racket/base
@flow-pict{
This is a paragraph.

This is another, with some @bold{interesting} @italic{elements.}
}}|

produces the following pict:

@examples[#:eval the-eval #:result-only #:label #f
@flow-pict{
This is a paragraph.

This is another, with some @bold{interesting} @italic{elements.}
}
]}


@section{Layers}


@examples[#:eval the-eval #:result-only #:label #f
(require (only-in (lib "scribble-slideshow/example/doc-demo.scrbl") [doc demo-doc])
         (only-in pict frame scale))
(for/list ([sp (in-list (scribble-slide-picts demo-doc))])
  (frame (scale sp 1/4)))
]


@section[#:tag "notes"]{Notes}

The interpretation of Scribble style names and style properties is incomplete,
and it is not customizable. It is sometimes inconsistent with slideshow's
defaults; for example, the default block width is wider.

The current hook for changing the base styles (@racket[current-sp-style]) is
likely to change in the future.

The default styles used by @racket[scribble-slides] are different from those
used by @racket[slide], so mixing the two will produce inconsistent-looking
results. The rendering of titles is especially different.

Titles are not baseline-aligned, so titles that result in picts of different
heights look inconsistent. This might be a slideshow issue.

Staging does not cooperate with slideshow's @tt{--condense} mode.

@(close-eval the-eval)
