#lang scribble/manual


@title{Racket Spark Light Documentation}

This section will expain each available feature in RSL.

@; ------------------------------------------------------------------------
@;table-of-contents[]
@include-section["getting-started.scrbl"]
@;include-section["rsl.scrbl"]

@section{Grammar}

@(racketgrammar*

  [Program
   Top-Level ...]

  [Top-Level
   Definition
   Expr]

  [Definition
   (define Id RExpr)
   (define-rsl-func (Id Id) RExpr)]

  [Expr
   RExpr
   TExpr]

  [RExpr RacketExpressions]

  [TExpr
   Datashell
   Transformation
   Reduction]

  [TFunc Id] ;; that references a TFunc defined with define-rsl-func

  [AFunc
   (lambda (Id Id) Expr)]

  [FilePath
   String]

  [Datashell
   (mk-datashell [Listof Any])
   (mk-datashell FilePath)
   Transformation]

  [Transformation
   (ds-map TFunc DataShell)
   (ds-filter TFunc Datashell)]

  [Reduction
   (ds-reduce AFunc Expr Datashell)
   (ds-collect Datashell)
   (ds-count Datashell)
   (ds-take-n Datashell num)])

A RacketExpression is any expression valid in #lang racket.


@section{Creating A Datashell}

A @bold{Datashell} is an immutable structure that stores user inputted lists of data,
on which RSL Operations are performed.

You will able to create a Datashell with the following functions:


@defform[(mk-datashell l)
         #:contracts([l list?])]{
 Creates a Datashell from the given list.
}

Example:
@racketblock[(define a (mk-datashell '(5 2)))]

@defform[(mk-datashell path)
         #:contracts([path String?])]{
 Creates a Datashell from the given csv file path.
}

Example:
@racketblock[(define csv-a (mk-datashell "../path-to-file.csv"))]

@para{All Transformation Functions (TFuncs) take one input, and have one output}

@section{Transformations}

@subsection{Defining Transformation Functions}

Transformations onto a Datashell are @bold{lazily} evaluated.

A Transformation function provides a mapping from one item to another item.

@defform[(define-rsl-func (name arg) body)
         #:contracts([name Id?]
                     [arg Id?]
                     [body Expr?])]{
 Creates a transformation function which can be passed into @bold{ds-map} or @bold{ds-filter}.
 Note again that @bold{define-map-func} only takes one input argument and produces one output.
 @para{@bold{Effects (such as print statements) will not occur until the Datashell this TFunc is
  applied to is passed to a Reduction.}}
}

Example:
@racketblock[
 (define-rsl-func (sub-8 z)
   (- z 8))
 (define-rsl-func (is-even num)
   (= (modulo num 2) 0))
]

@subsection{Mapping Transformation Functions}

When called on a Datashell, transformations are queued up for later execution.
These transformations will not execute until a Reduction is called on the Datashell.
This allow RSL to evaluate all transformations applied to the Datashell in one iteration.

@defform[(ds-map tfunc datashell)
         #:contracts([tfunc tfunc?]
                     [datashell Datashell?])]{
 Creates a new Datashell using the given Datashell mapped with the given function.
 The given procedure is not evaluated until a reduction is executed.
}

Example:
@racketblock[
 (define-rsl-func (sub-8 z)
   (- z 8))

 (define b (ds-map sub-8 (mk-datashell '(50 20 49))))
 b
 >'(42 12 41)]

@defform[(ds-filter tfunc datashell)
         #:contracts([tfunc tfunc?]
                     [datashell Datashell?])]{
 Creates a new Datashell using the given Datashell filtered with the given predicate function.
 The given procedure is not evaluated until a reduction is executed.
}

Example:
@racketblock[
 (define-rsl-func (length-greater-than-3? l)
   (> (length l) 3))

 (define filtered (ds-filter length-greater-than-3?
                             (mk-datashell '((list 1 2 5 6)
                                             (list 1 2)
                                             (list 3 6 2 9 0)))))
 filtered
 >'((list 1 2 5 6) (list 3 6 2 9 0))]

@section{Reductions/Actions}

Reductions are @bold{eagerly} evaluated.

A Reduction immediatly triggers the evaluation of the datashell's queued transformation(s), and it reduces the transformed dataset into a value to return
to the user.

@defform[(ds-reduce afunc base datashell)
         #:contracts([afunc Procedure?]
                     [base Any?]
                     [datashell Datashell?])]{
 Applies the datashell's queued transformations then immediatly evaluates and returns the result of the reduction function on the datashell.

 The return type of @bold{ds-reduce} can be any except a Datashell.
 The return type of ds-reduce is the same as the type of @bold{base}, and the return type of @bold{afunc}.
}

Example:
@racketblock[
 (define a (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))
 (ds-reduce + 0 a)
 > 55
]

@subsection{Library Reductions}


@defform[(ds-collect datashell)
         #:contracts([datashell Datashell?])]{
 Returns a list from the given datashell by applying all the datashell's queued functions and immediatly collecting the results.

 @bold{ds-collect} returns a [Listof Any].
}

Example:
@racketblock[
 (define a2 (ds-filter mult-10 (ds-map less-than-5?
                                       (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))))
 (define abcd2 (ds-filter multiple-of-20? a2))

 (ds-collect abcd2)
 > (list 20 40)
]

@defform[(ds-count datashell)
         #:contracts([datashell Datashell?])]{
 Counts the number of items in the list after all transformations and filters have been applied.

 @bold{ds-collect} returns a Number.
}

Example:
@racketblock[
 (define a2 (ds-map mult-10 (ds-filter less-than-5?
                                       (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))))
 (define abcd2 (ds-filter multiple-of-20? a2))

 (ds-count abcd2)
 > 2
]

@defform[(ds-take-n datashell num)
         #:contracts([datashell Datashell?]
                     [num Integer?])]{
 Gets the first n items in the list after all transformations and filters have been applied.

 @bold{ds-collect} returns a [Listof Any] of at most n items.
}

Example:
@racketblock[
 (define a2 (ds-filter even-num? (ds-map mult-5
                                         (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))))
 (ds-take-n abcd2 3)
 > '(10 20 30)
]
