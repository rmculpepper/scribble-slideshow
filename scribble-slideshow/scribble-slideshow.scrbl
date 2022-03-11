#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract scribble-slideshow
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

@defmodule[scribble-slideshow #:lang]


@; ============================================================
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


@; ============================================================
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


@; ------------------------------------------------------------
@subsection[#:tag "style"]{Styles}



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
