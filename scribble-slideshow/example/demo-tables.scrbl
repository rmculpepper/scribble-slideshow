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
          (only-in slideshow [slide s:slide] [para s:para])
          pict/shadow
          scribble/core
          scribble/base
          scribble/manual
          scribble-slideshow/util/columns
          (for-label racket/base))

@(begin
   ;; Here we change some base style defaults.
   (current-sp-style (hash-set* (current-sp-style) 'justify #t 'debug '(linebreak)))
   (define talk-style
     (style #f (list 'widescreen (style-diffs `((istyle text-base roman)))))))

@title[#:style talk-style]{Demo}

@section{Columns}

@columns[
#:style (style #f (list (style-diffs `((istyle text-size 24)))))

@column[#:width 1 #:valign 'top #:hmargin 5
        #:style 'border]{
On the @emph{left} side, we have some text, and some more text, and blah blah.
}

@column[#:width 2 #:valign 'vcenter #:hmargin 5
        #:style (style #f (list 'border (style-diffs '((istyle bgcolor "yellow")))))]{
On the @bold{right} side, the text goes on and on. There's a lot of
text. @emph{Lorem ipsum} levels of text, if you know what I mean.
}

]

@(p:blank)

@centered[
@compound*[
#:style (style #f (list (style-diffs '((istyle block-width 900 block-halign left)))))

@columns[
@column[#:style (style #f (list (background-color-property "yellow")))]{
Here is one row.
}

@column[#:style (style #f (list (background-color-property "pink")))]{
Here is another row.
}

]

]]


@section{Blue box}

@defproc[(foo [bar quux?]) snark?]{

Okay.}

@section{Vertical Alignment}

@tabular[
#:column-properties '((top) (topline) (vcenter) (baseline) (bottom))

(list
 (list @para{@larger{top}}
       @para{topline}
       @para{vcenter}
       @para{baseline}
       @para{@smaller{bottom}})

 (list @para{@smaller{top}}
       @para{@larger{topline}}
       @para{vcenter}
       @para{baseline}
       @para{@larger{bottom}}))

]
