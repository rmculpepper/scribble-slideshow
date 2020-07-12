#lang at-exp racket/base
(require (except-in slideshow/base item para t tt)
         (except-in pict table)
         pict/shadow
         scribble-slideshow
         scribble/core
         scribble/base
         scribble/manual
         scribble/html-properties
         scribble/latex-properties
         (for-label racket/base))

(define (blue . content)
  (apply elem #:style (style #f (list (color-property "blue"))) content))

(define (on-pink . content)
  (apply elem #:style (style #f (list (background-color-property "pink"))) content))

(define (shadow-style rad [dx 0] [dy dx])
  (style #f (list (text-post-property (lambda (p) (shadow p rad dx dy #:shadow-color "purple"))))))

(slide
 @flow-pict[#:style 'roman]{
   This whole slide consists of a @italic{flow}. It consists of
   multiple @tt{paragraphs} and @elem[#:style 'sf]{other such stuff}.

   @para[#:style (style #f (list (background-color-property "yellow")))]{
   This is a @italic{paragraph}. It is written using @blue{Scribble's
   @litchar["@"]-exp reader}, which means that when I use @racket[para] and
   @racket[it] and picts, @on-pink{I do not have to break things @elem[#:style 'larger]{manually}},
   like @racket[(para "This" (it "is") "a para")]---I can write them @on-pink{``more naturally''}.
   }

   This @racket[λ] is good stuff:
   @itemlist[
   @item{it is @elem[#:style (shadow-style 5 2)]{@italic{functional@elem[#:style 'superscript]{ish}}}}
   @item{it is @elem[#:style (shadow-style 5 2)]{@italic{higher-order}@elem[#:style 'subscript]{for sure}}}
   ]
   })

(slide
 @flow-pict[#:style 'roman]{
   This is some centered text within a paragraph:
   @centered{carpe diem}
   and this is what comes after.

   And this is a whole new paragraph.@margin-note{With a margin note!}

   @defproc[(foo [bar baz?]) quux/c]{
   Returns the best @italic{quux} appropriate for @racket[bar].
   }
 })

(slide
 @frame[
 @flow-pict[#:style 'roman]{
   @; -- Needs table!
   @racketblock[
   (define (map f xs)
     (if (pair? xs)
         (cons (f (car xs)) (map f (cdr xs)))
         '()))
   ]
   }]
 @frame[
 @flow-pict[#:style 'roman]{
   @racketgrammar*[
     #:literals (lambda)
     [X #, @elem{variable name}]
     [E X (lambda (X) E) (E E)]]
 }])

(slide
 @flow-pict[#:style 'roman]{

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
 })
