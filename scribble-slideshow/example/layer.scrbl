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
   (style #f (list (style-diffs '((iset text-base roman justify #t debug (layer #;linebreak)))))))

@title[#:style talk-style]{Demo of layers}

Here is some text.
I'm talking a lot!

@(define my-layer
   (slide-layer 'rb #:base 'full
     #:style (style 'shrinkwrap
                    (list (style-diffs
                           `((nset bgcolor "pink"
                                   block-padding 4
                                   block-post ,(list p:frame))))))))

@compound*[#:layer my-layer]{
@(p:colorize (p:disk 50) "red")
}
