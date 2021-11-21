;; This part just uses plain Scribble forms (although with styles that only
;; scribble-slideshow recognizes), so it can use the scribble/manual language.

#lang scribble/manual

@; This file demonstrates staging. A part style of 'next or 'alts applies to
@; that part's immediate subparts. For example, since this file's @title part
@; has the style 'next, then all sections in this file (but not subsections)
@; incrementally add to the content immediately belonging to the title.

@title{An example of staging}

This slide will have several paragraphs.

@section[#:style 'alt]{..}     @; The special title ".." means reuse previous.

Here is the first addition, added using @racket['alt], so it will be
retracted at the end of this @emph{section}.

@subsection[#:style 'next]{..}

This @racket['next] addition is in a @emph{subsection}, though, so it
gets to build on the previous slide's content before it is retracted.

@subsection[#:style 'digress]{A majestik digression}

Mynd you, møøse bites Kan be pretti nasti.

@section[#:style 'next]{..}

Whoops, I take that back. We'll go with this one instead. I like this one better.

@section[#:style 'next]{Staging is finished!}

This is the last one. Notice the title is different.
