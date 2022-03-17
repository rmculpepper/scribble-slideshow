#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/draw
                     scribble-slideshow/pict scribble-slideshow/slideshow
                     scribble/base scribble/core scribble/manual scribble/decode
                     (except-in pict table) pict/shadow
                     (only-in slideshow slide)))

@(module stx racket/base
   (require racket/string racket/port syntax/strip-context syntax/modread
            (prefix-in s: scribble/reader))
   (provide stx-strings->datums)
   (define (stx-strings->datums ctx strs)
     (with-module-reading-parameterization
       (lambda ()
         (define s (string-join (syntax->datum strs) ""))
         (port->list (lambda (in) (replace-context ctx (s:read-syntax 'example in)))
                     (open-input-string s))))))

@(begin
   (require (for-syntax racket/base 'stx))
   (define-syntax (code-example stx)
     (syntax-case stx ()
       [(_ str ...)
        (with-syntax ([(datum ...) (stx-strings->datums stx #'(str ...))]
                      [lang-line (syntax-local-introduce #'"#lang at-exp racket/base")])
          #'(list (codeblock #:keep-lang-line? #f lang-line "\n" str ...)
                  (examples #:eval the-eval #:result-only #:label #f datum ...)))]))
  (define-syntax (slides-example stx)
     (syntax-case stx ()
       [(_ str ...)
        (with-syntax ([(datum ...) (stx-strings->datums stx #'(str ...))])
          #'(list (codeblock #:keep-lang-line? #t str ...)
                  (examples #:eval the-eval #:result-only #:label #f
                            (mod->slide-picts 'datum ...))))])))

@(define (s-tech . pc)
   (apply tech #:doc '(lib "scribblings/scribble/scribble.scrbl") pc))

@(define (p-tech . pc)
   (apply tech #:doc '(lib "pict/scribblings/pict.scrbl") pc))

@(define repo "https://github.com/rmculpepper/scribble-slideshow/tree/master")

@(define the-eval (make-base-eval))
@(the-eval '(require scribble/base scribble/core scribble/manual
                     scribble-slideshow/pict
                     (except-in pict table) pict/shadow))

@(the-eval
  '(define (mod->slide-picts mod-decl)
     (define mod (gensym))
     (parameterize ((current-module-declare-name (make-resolved-module-path mod)))
       (eval mod-decl))
     (define doc (dynamic-require (list 'quote mod) 'doc))
     (for/list ([sp (in-list (scribble-slide-picts doc))])
       (frame (scale sp 1/4)))))

@title[#:tag "scribble-slideshow"]{scribble-slideshow: Using Scribble to Make Slides}

This library provides both a @emph{language} and a @emph{library} for writing
slideshows using Scribble notation. (To clarify, I don't mean just using the
@racketmodname[at-exp] reader with @racketmodname[slideshow]. I mean that the
slides are written using Scribble forms, evaluated to Scribble document
structures, and then rendered to slides and picts.) One benefit is that element
styles cooperate automatically with line-breaking.

The @racketmodname[scribble-slideshow] @emph{language} allows users to write
Scribble documents that can automatically be run as slideshows.

The @racketmodname[scribble-slideshow] @emph{library} provides functions like
@racket[flow-pict] and @racket[scribble-slides] that convert Scribble
@s-tech{pre-flow} into @p-tech{picts} and emit slides from Scribble
@s-tech{parts}, respectively.

See the @hyperlink[(format "~a/scribble-slideshow/example" repo)]{scribble-slideshow/examples}
directory for extended, runnable examples.


@; ============================================================
@section[#:tag "scribble-slide-lang"]{Scribble Language for Slideshows}

@defmodule[scribble-slideshow #:lang]

A language providing the bindings of @racketmodname[racket/base] and
@racketmodname[scribble-slideshow], except that Racket's @racket[#%module-begin]
form is replaced with a variant with the following behavior:
@itemlist[

@item{Like the @racketmodname[scribble/base] and @racketmodname[scribble/manual]
languages, the contents of the module (minus requires, definitions, etc) are
automatically gathered and interpreted as a Scribble document (a
@s-tech{part}). The module defines and exports the name name @racketvarfont{doc}
holding the document.}

@item{The language emits a @tt{main} submodule that converts the Scribble
document to slides.}
]


@; ============================================================
@section[#:tag "scribble-slide"]{Scribble to Slides}

@defmodule[scribble-slideshow/slideshow]

@defproc[(scribble-slides [pre-part pre-part?] ...) void?]{

Decodes the @racket[pre-part]s to a Scribble @s-tech{part} (see @racket[decode])
and generates slides from its content and sub-parts. That is, there is an
additional slide generated for each section, sub-section, etc.

The style of each @racket[part] can be used to control a slide's layout and
staging. See the extended examples for more details.
}


@; ============================================================
@section[#:tag "scribble-pict"]{Scribble to Picts}

@defmodule[scribble-slideshow/pict]

The exports of @racketmodname[scribble-slideshow/pict] are also available from
@racketmodname[scribble-slideshow]. Unlike @racketmodname[scribble-slideshow],
@racketmodname[scribble-slideshow/pict] avoids a direct dependency on the
@racketmodname[slideshow] module, so it can be used in context where the GUI is
disallowed.

@defproc[(flow-pict [pre-flow pre-flow?] ...)
         pict?]{

Decodes the @racket[pre-flow]s into Scribble @s-tech{flow} (see
@racket[decode-flow]) and then converts it to a pict.

For example:

@code-example|{
@flow-pict{
This is a paragraph.

This is another, with some @bold{interesting} @italic{elements.}
}}|
}

@defproc[(scribble-slide-picts [part part?]) (listof pict?)]{


@slides-example|{
#lang scribble/manual

@title{Fruits}

@section{Apples}

Apples can be
@itemlist[
@item{red, or}
@item{yellow, or}
@item{green.}
]

@section{Bananas}

Bananas are yellow when ripe.

@subsection[#:style 'alt]{..}

Before that, they are green.

@subsection[#:style 'next]{..}

They turn black when overripe.

}|

}

@defproc[(part/make-slides [proc (-> void?)]) part?]{

Creates a @s-tech{part} wrapping a procedure. The procedure is run for
effect when the part is processed by @racket[scribble-slides] or its
equivalent; it should call @racket[slide] or similar functions to emit
slides for that part of the document.
}


@; ------------------------------------------------------------
@subsection[#:tag "style"]{Styles}

The translation from Scribble structures to picts is controlled by the current
@deftech{sp-style} (Scribble to Pict style), which is represented by an
immutable hash with symbol keys.

The following keys are relevant to the translation of Scribble @s-tech{content}:
@itemlist[

@item{@racket['text-base], @racket['text-mods] --- These are combined to get the
style argument of @racket[text].}

@item{@racket['text-size] --- An integer between 1 and 1024. See @racket[text].}

@item{@racket['white-space] --- One of the following: @racket[#f] (normal
breaking and collapsing), @racket['pre] (no break, no collapse),
@racket['pre-wrap] (break, no collapse), @racket['nowrap] (collapse, no break).}

@item{@racket['color] --- A string or @racket[color%]. Used to @racket[colorize]
each pict produced by Scribble @s-tech{content}.}

@item{@racket['scale] --- A real number used to @racket[scale] each pict
produced to represent Scribble @s-tech{content}.}

@item{@racket['text-post] --- A list of pict-to-pict transformers, applied in
order to each pict produced to represent Scribble text.}

@item{@racket['elem-post] --- A list of pict-to-pict transformers, applied in
order to each pict produced to represent any Scribble @s-tech{content}.}

@item{@racket['bgcolor] --- A string or @racket[color%].}

]

The following keys are relevant to the translation of Scribble @s-tech{blocks}
and @s-tech{flow}:
@itemlist[

@item{@racket['block-width] --- A positive real.}

@item{@racket['block-border] --- A list of symbols in @racket['all],
@racket['left], @racket['right], @racket['top], @racket['bottom].}

@item{@racket['bgcolor] --- @racket[#f] or a string or @racket[color%].}

@;{Not described: inset-to-width?, block-halign, block-inset}

@item{@racket['block-sep] --- A nonnegative real, used for the spacing between
blocks.}

@item{@racket['line-sep] --- A nonnegative real, used for the spacing between
lines in a paragraph and between items in a @racket['compact]
@s-tech{itemization}.}

]

The following keys are relevant to the translation of Scribble @s-tech{parts}:
@itemlist[

@item{@racket['slide-title-base], @racket['slide-title-size],
@racket['slide-title-color] --- Like @racket['text-base], @racket['text-size],
and @racket['color], but applied to the slide title.}

]


@defthing[current-sp-style parameter?]{

@bold{Deprecated.} Use a @racket[style-transformer] in the style of the root
@s-tech{part} of the Scribble document to set the initial styles instead.
}

@defproc[(style-transformer [update (-> hash? hash?)]) any/c]{

Returns an opaque value suitable as a Scribble @(styleprop). 

}

@(define (styleprop) (s-tech "style property"))

@defproc[(text-post-property [process (-> pict? pict?)]) any/c]{

Returns an opaque value suitable as a Scribble @(styleprop). When the style
property is applied to Scribble content, each pict that is produced from an
embedded string is post-processed with @racket[process]. Text post-processors
are applied innermost-first, and all text post-processors are applied before any
element post-processor.

Note that strings may be split into many pieces as part of the line-breaking
process; the @racket[process] function is applied to each piece individually.

@code-example|{
@(define (tx-elem f . content)
   (apply elem #:style (style #f (list (text-post-property f))) content))
@(define ((shadow-tx rad dxy [color "purple"]) p)
   (shadow p rad dxy dxy #:shadow-color color))
@flow-pict{
Here is some @tx-elem[(shadow-tx 4 2)]{cool @disk[20] text}.
}}|

Note that the separating pict has no shadow.
}

@defproc[(elem-post-property [process (-> pict? pict?)]) any/c]{

Returns an opaque value suitable as a Scribble @(styleprop). When the style
property is applied to Scribble content, each pict that is produced from the
@s-tech{content} is post-processed with @racket[process]. All element
post-processors are applied innermost-first.

Note that content may be split into many pieces as part of the line-breaking
process; the @racket[process] function is applied to each piece individually.

@code-example|{
@(define (ex-elem f . content)
   (apply elem #:style (style #f (list (elem-post-property f))) content))
@flow-pict{
Here is some @ex-elem[(shadow-tx 4 2)]{cool @disk[20] text}.
}}|

Note the shadow behind the separating pict.
}


@; ------------------------------------------------------------
@subsection{Layers}

@;{
         in-style
         in-layer
         layer
         slide-layer
         slide-zone
}

@examples[#:eval the-eval #:result-only #:label #f
(require (only-in (lib "scribble-slideshow/example/doc-demo.scrbl") [doc demo-doc])
         (only-in pict frame scale))
(for/list ([sp (in-list (scribble-slide-picts demo-doc))])
  (frame (scale sp 1/4)))
]


@; ============================================================
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


@; ============================================================

@(close-eval the-eval)
