;; This part just uses plain Scribble forms (although with styles that only
;; scribble-slideshow recognizes), so it can use the scribble/manual language.

#lang scribble/manual

@; This file demonstrates staging. A part style of 'next or 'alts applies to
@; that part's immediate subparts. For example, since this file's @title part
@; has the style 'next, then all sections in this file (but not subsections)
@; incrementally add to the content immediately belonging to the title.

@title[#:style 'next]{An example of staging}

This slide keeps adding paragraphs...

@section[#:style 'alts]{..}     @; The special title ".." means reuse previous.

Here is another.

@subsection{..}

Whoops, I take that back. We'll go with this one instead. I like this one better.

@section{Staging finished}

This is the last one. Notice the title is different.
