#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/draw
                     scribble-slideshow/pict scribble-slideshow/slideshow
                     scribble/base scribble/core scribble/manual scribble/decode
                     (except-in pict table) pict/shadow
                     ppict/align ppict/pict ppict/zone
                     (only-in slideshow slide margin title-h current-gap-size)))

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

@(define (pp-tech . pc)
   (apply tech #:doc '(lib "ppict/ppict.scrbl") pc))

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
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This package provides both a @emph{language} and a @emph{library} for writing
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

@slides-example|{
#lang scribble-slideshow
@(require scribble/base)

@title{Minimal Example}

This is a @emph{minimal example},
a document that creates a single slide.
}|

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
@s-tech{part}). The module defines and exports the name @racketvarfont{doc}
holding the document.}

@item{The language emits a @tt{main} submodule that converts the Scribble
document to slides and presents them as a slideshow.  The @tt{main} submodule
depends on @racketmodname[scribble-slideshow/slideshow] and the Racket GUI
library, but the enclosing module does not.}

]


@; ============================================================
@section[#:tag "scribble-slide"]{Scribble to Slides}

@defmodule[scribble-slideshow/slideshow]

The exports of @racketmodname[scribble-slideshow/slideshow] are @emph{not}
provided by @racketmodname[scribble-slideshow].

@defproc[(scribble-slides [pre-part pre-part?] ...) void?]{

Decodes the @racket[pre-part]s to a Scribble @s-tech{part} (see @racket[decode])
and generates slides from its content and sub-parts. That is, there is an
additional slide generated for each section, sub-section, etc.

The style of each @racket[part] can be used to control a slide's layout and
staging. See @secref["style"], and see the extended examples for more details.
}

@defproc[(scribble-slides* [part part?]) void?]{

Generates slides from the content and sub-parts of @racket[part].
}


@; ============================================================
@section[#:tag "scribble-pict"]{Scribble to Picts}

@defmodule[scribble-slideshow/pict]

The exports of @racketmodname[scribble-slideshow/pict] are also available from
@racketmodname[scribble-slideshow]. Unlike @racketmodname[scribble-slideshow],
@racketmodname[scribble-slideshow/pict] avoids a direct dependency on the
@racketmodname[slideshow] module, so it can be used in context where the GUI is
disallowed.

@defproc[(flow-pict [pre-flow pre-flow?] ...
                    [#:style style (or/c #f style? symbol? string?) plain])
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

Like @racket[scribble-slides*], but returns a list of picts instead of
presenting them as slides.

Since this procedure does not depend on @racketmodname[slideshow], it is not
affected by changes to slideshow configuration like the margin and title height.
}

@defproc[(part/make-slides [proc (-> void?)]) part?]{

Creates a @s-tech{part} wrapping a procedure. The procedure is run for
effect when the part is processed by @racket[scribble-slides] or its
equivalent; it should call @racket[slide] or similar functions to emit
slides for that part of the document.
}


@; ------------------------------------------------------------
@subsection[#:tag "style"]{Styles}

The translation from Scribble structures to picts and slides is controlled by
the current @deftech{sp-style} (Scribble to Pict style), which contains three
kinds of data:
@itemlist[

@item{@emph{inherited} sp-style settings with symbolic keys, which directly
affect the generation of picts from Scribble data, such as the
@racket['text-size] key which determines the current font size, and}

@item{@emph{non-inherited} sp-style settings with symbolic keys, which directly
affect the generation of picts from a single Scribble structure, such as the
@racket['block-padding] key which determines extra space added around a block
pict, and}

@item{a @tech{style mapping} (stored in the the @racket['styles] key) which
translates a Scribble @s-tech{style name} to an update of the other
@tech{sp-style} settings}

]
The following subsections describe some of the @tech{sp-style} keys.

@defthing[current-sp-style parameter?]{

@bold{Deprecated.} Use a @racket[style-transformer] in the style of the root
@s-tech{part} of the Scribble document to set the initial styles instead.
}


@; ----------------------------------------
@subsubsection[#:tag "content-style"]{Translation of Scribble Content}

The following @tech{sp-style} keys are relevant to the translation of
Scribble @s-tech{content}:
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
order to each pict produced to represent Scribble text. See also
@racket[text-post-property].}

@item{@racket['elem-post] --- A list of pict-to-pict transformers, applied in
order to each pict produced to represent any Scribble @s-tech{content}. See also
@racket[elem-post-property].}

@item{@racket['bgcolor] --- A string or @racket[color%].}

]

The initial @tech{style mapping} contains entries for common @s-tech{style
names} for Scribble content. For example:
@itemlist[

@item{@racket['sf] sets the @racket['text-base] key to @racket['swiss]}

@item{@racket['larger] multiplies the @racket['scale] key's value by @racket[3/2]}

@item{@racket["RktVal"] (produced by @racket[racket], @racket[code], etc) sets
the @racket['text-base] key to @racket['modern] and the @racket['color] key to
@racket['(@#,racketvalfont{#x22 #x8B #x22})]}

@item{and so on}
]

The following Scribble @s-tech{style properties} are recognized and handled:
@racket[color-property] and @racket[background-color-property].

@; ----------------------------------------
@subsubsection[#:tag "block-style"]{Translation of Scribble Flow}

The following @tech{sp-style} keys are relevant to the translation of Scribble
@s-tech{blocks} and @s-tech{flow}:
@itemlist[

@item{@racket['block-width] --- A positive real, may be @racket[+inf.0] to
disable linebreaking.}

@item{@racket['block-halign] --- One of @racket['left], @racket['right], or
@racket['center].}

@item{@racket['block-sep] --- A nonnegative real, used for the spacing between
blocks.}

@item{@racket['line-sep] --- A nonnegative real, used for the spacing between
lines in a paragraph.}

@item{@racket['bgcolor] --- @racket[#f] or a string or @racket[color%]. When
this style is applied to a block, the background color is applied to the whole
block, and the style is not inherited by the block's contents.}

@;{ NStyle keys:
@item{@racket['block-border] --- A list of symbols in @racket['all],
@racket['left], @racket['right], @racket['top], @racket['bottom].}
}

]

The initial @tech{style mapping} contains entries for common @s-tech{style
names} for Scribble flow. For example:
@itemlist[

@item{@racket['compact] sets the @racket['block-sep] key to the current value of
the @racket['line-sep] key (and it works for any Scribble @s-tech{block}, not
just @racket[itemization]s)}

]

The following Scribble @s-tech{style properties} are recognized and handled:
@racket[color-property], @racket[background-color-property],
@racket[table-columns], and @racket[table-cells].

@; ----------------------------------------
@subsubsection[#:tag "part-style"]{Translation of Scribble Parts}

The initial @tech{style mapping} contains one pseudo-@s-tech{style name} for
Scribble parts:
@itemlist[

@item{@racket['slide-title] sets various @s-tech{content}-related keys to style
the part's title}

]

The following custom @s-tech{style properties} are recognized and handled:
@itemlist[

@item{@racket['next], @racket['alt], and @racket['digress] --- Sets the
@seclink["staging-mode"]{staging mode} of the current part.}

@item{@racket['auto], @racket['center], @racket['top], @racket['tall] --- Sets
the current slide's layout.}

@item{@racket['fullscreen], @racket['widescreen] --- Sets the current slide's
aspect ratio.}

@item{@racket['ignore] --- Do not generate a slide for this part's immediate
contents, but process the subparts normally.}

@item{@racket['ignore*] --- Do not generate any slides for this part, neither
for its immediate contents nor for its subparts.}

@item{@racket['no-title] --- Do not display the part's title.}

]

@; ----------------------------------------
@subsubsection[#:tag "style-style"]{Translation of Scribble Styles}

The following keys are relevant to the translation of Scribble @s-tech{styles}:
@itemlist[

@item{@racket['styles] --- A @deftech{style mapping}, a hash mapping Scribble
@s-tech{style names} (symbols and strings) to @tech{sp-style diff-list}.}

]

An @deftech{sp-style diff-list} is a list of @tech{sp-style diff}s. A
@deftech{sp-style diff} is one of the following:
@itemlist[

@item{@racket[(list 'istyle _key _value ... ...)] --- Sets each @racket[_key] to
the corresponding @racket[_value].}

@item{@racket[(list 'update _key _default _update)] --- Updates @racket[_key] by
applying @racket[_update] to its existing value (or to @racket[_default]) if the
key has no existing value.}

@item{@racket[(list 'ref _style-name)] --- Applies the changes of the style
named @racket[_style-name] in the current @tech{style mapping}.}

@item{@racket[(list 'stylemap _style-name _style-diff-list ... ...)] --- Updates
the current @tech{style mapping}, adding or replacing entries for the given
@racket[_style-name]s.}

@;{
@item{@racket[(list 'nstyle key value ... ...)] --- ?}
@item{@racket[(list 'nstyle-update key value ... ...)] --- ?}
}

]

@(define (styleprop) (s-tech "style property"))

@defproc[(style-diffs [update @#,tech{sp-style diff-list}]) any/c]{

Returns an opaque value suitable as a Scribble @(styleprop). When the style
property is applied to a Scribble structure, it updates the current
@tech{sp-style} used for that process that part of the document.
}

@defproc[(text-post-property [process (-> pict? pict?)]) any/c]{

Returns an opaque value suitable as a Scribble @(styleprop). When the style
property is applied to Scribble content, each pict that is produced from an
embedded string is post-processed with @racket[process]. Text post-processors
are applied innermost-first, and all text post-processors are applied before any
element post-processor.

Note that strings may be split into many pieces as part of the line-breaking
process; the @racket[process] function is applied to each piece individually.
Future versions of this library may change the way strings are split.

@code-example|{
@(define (tx-elem f . content)
   (apply elem #:style (style #f (list (text-post-property f))) content))
@(define ((shadow-tx rad dxy [color "purple"]) p)
   (shadow p rad dxy dxy #:shadow-color color))
@flow-pict{
@tx-elem[frame]{Here is some @tx-elem[(shadow-tx 4 2)]{cool @disk[20] text}.}
}}|

Note that the separating pict has no shadow and no frame.
}

@defproc[(elem-post-property [process (-> pict? pict?)]) any/c]{

Returns an opaque value suitable as a Scribble @(styleprop). When the style
property is applied to Scribble content, each pict that is produced from the
@s-tech{content} is post-processed with @racket[process]. All element
post-processors are applied innermost-first.

Note that content may be split into many pieces as part of the line-breaking
process; the @racket[process] function is applied to each piece individually.
Future versions of this library may change the way strings are split.

@code-example|{
@(define (ex-elem f . content)
   (apply elem #:style (style #f (list (elem-post-property f))) content))
@flow-pict{
Here is some @ex-elem[(shadow-tx 4 2)]{cool @disk[20] text}.
}}|

Note the shadow behind the separating pict.
}


@; ------------------------------------------------------------
@subsection[#:tag "staging-mode"]{Slide Staging Modes}

A part can build on the state of the previous part. This state includes the
title, the contents of the slide (both on the main @tech{layer} and any other
@tech{layers}). A part can also control what state it leaves to the next part. A
staging mode represents a combination of these two choices:
@itemlist[

@item{@racket[#f] (default) --- Start with empty state. Leave current state for
next part.}

@item{@racket['next] --- Start with inherited state. Leave current state for next
part.}

@item{@racket['alt] --- Start with inherited state. Reset to inherited state for
next part.}

@item{@racket['digress] --- Start with empty state. Reset to inherited state for
next part.}

]
Note that @racket['alt] and @racket['digress] only reset the state @emph{after}
the part's subparts are processed.

If the part's title is the string @racket[".."], the slide uses the title from
the inherited state.

For example:

@slides-example|{
#lang scribble/manual

@title[#:style 'ignore]{Fruits}

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

@subsubsection[#:style 'next]{..}

Well, yellow-green.

@subsection[#:style 'digress]{Commercial break}

@centered{Shop smart! Shop @bold{S-MART}!}

@subsection[#:style 'next]{..}

They turn brown when overripe.

}|


@; ------------------------------------------------------------
@subsection{Layers}

A slide can be composed of contents on different @deftech{layers}. A slide layer
consists of a zone (@racket[zone?]) (which determines the block width for
linebreaking), a placer (@racket[placer?]), a Z-order, and various
styling-related settings. Layers cooperate with @seclink["staging-mode"]{slide
staging}.

@slides-example|{
#lang scribble/manual
@(require scribble-slideshow ppict/zone (only-in pict frame))

@(define main-layer
   (slide-layer 'lc (coord-zone 0.2 0 0.8 1) #:base 'main))
@(define side-layer
   (slide-layer 'rb
                #:style (style-diffs
                         `((iset text-size 20 color "blue")
                           (nset block-post ,(list frame))))))

@title{An enumeration of ponderous considerations}

@compound*[#:layer main-layer]{
In the final analysis, there are many important points to consider.
@itemlist[
@item{There is @emph{this} point.}
@item{And there is @bold{that} point.}
]}

@compound*[#:layer side-layer]{
The points above notwithstanding, the contrary position is also defensible.
}

@section[#:style 'next]{..}

@compound*[#:layer main-layer]{
That these arguments are compelling is self-evident.
}

@compound*[#:layer side-layer]{
But only in the sense that the benighted continue to attempt to defend it.
}
}|

@defproc[(layer? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{layer}, @racket[#f] otherwise.
}

@defproc[(layer [align/placer (or/c placer? align/c)]
                [zone zone?]
                [#:z z real? 1]
                [#:style style style-update/c '()]
                [#:pre-decorate pre-decorate (or/c #f (-> pict? pict?)) #f]
                [#:post-decorate post-decorate (or/c #f (-> pict? pict?)) #f])
         layer?]{

Creates a @tech{layer} that places its content according to
@racket[align/placer] within @racket[zone] relative to the slide.

If @racket[align/placer] is an alignment, it is converted to a placer using
@racket[aligned-placer].

If @racket[pre-decorate] is a procedure, it is called on the contents of the
layer for the current @seclink["staging-mode"]{stage}. If
@racket[post-decorate] is a procedure, it is called on the (pre-decorated)
contents of the layer after they are inset to the maximum size predicted for all
related stages.
}

@defproc[(slide-layer [align/placer (or/c placer? align/c)]
                      [zone (or/c #f zone?) #f]
                      [#:base base slide-zone-symbol/c 'body]
                      [#:z z real? 1]
                      [#:style style style-update/c '()]
                      [#:pre-decorate pre-decorate (or/c #f (-> pict? pict?)) #f]
                      [#:post-decorate post-decorate (or/c #f (-> pict? pict?)) #f])
         layer?]{

Equivalent to @racket[(layer align/placer (subzone zone (slide-zone base)) ....)].
}

@defproc[(slide-zone [name symbol?]
                     [#:aspect aspect aspect/c #f])
         zone?]{

Returns a zone corresponding to one of the following areas of the slide,
depending on @racket[name]:
@itemlist[

@item{@racket['main] --- Centered zone whose top edge is @racket[(+ margin
title-h (* 2 (current-gap-size)))] units from the top of the screen.}

@item{@racket['tall-main] --- Centered zone whose top edge is @racket[(+ margin
title-h (* 1 (current-gap-size)))] units from the top of the screen.}

@item{@racket['full] --- Centered zone whose top and bottom edges are
@racket[margin] units from the edges of the screen.}

@item{@racket['screen] --- Centered zone whose edges are the same as the edges
of the screen.}

@item{@racket['body] --- Non-centered zone whose top edge is @racket[(+ margin
title-h (* 2 (current-gap-size)))] units from the top of the screen and whose
bottom edge is @racket[margin] units from the bottom of the screen.}

@item{@racket['tall-body] --- Non-centered zone whose top edge is @racket[(+
margin title-h (* 1 (current-gap-size)))] units from the top of the screen and
whose bottom edge is @racket[margin] units from the bottom of the screen.}

@item{@racket['title] --- Non-centered zone whose top edge is @racket[margin]
units from the top of the screen and whose height is @racket[title-h].}

@item{@racket['main/full] --- Title-dependent zone that is like @racket['main]
if a title is present and like @racket['full] if there is no title present.}

]

Except for @racket['screen], the left and right edges are @racket[margin] units
from the edges of an @racket[aspect]-dimensioned screen.
}

@defproc[(in-layer [#:layer layer layer?]
                   [pre-flow pre-flow?] ...)
         block?]{

Decodes @racket[pre-flow] and marks the resulting blocks for inclusion
in the layer @racket[layer].
}


@; ============================================================
@section[#:tag "notes"]{Notes}

The interpretation of Scribble style names and style properties is
incomplete, and the interpretation of style properties is not
customizable. It is sometimes inconsistent with slideshow's defaults;
for example, the default block width is wider.

The default styles used by @racket[scribble-slides] are different from those
used by @racket[slide], so mixing the two will produce inconsistent-looking
results. The rendering of titles is especially different.

Staging does not cooperate with slideshow's @tt{--condense} mode.


@; ============================================================

@(close-eval the-eval)
