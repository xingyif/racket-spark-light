#lang scribble/manual


@title{Racket Spark Light Documentation}

This section will expain each available feature in RSL.


@section{Grammer}

@(racketgrammar*

  [Program
   Top-Level ...]

  [Top-Level
   Definition
   Expr]

  [Definition
   (define Id RExpr)
   (define-map-func (Id Id) RExpr)
   (define-filter-pred (Id Id) RExpr)
   (save-ds Id Datashell)]

  [Expr
   RExpr
   TExpr]

  [RExpr
   "Racket expressions"]

  [TExpr
   Datashell
   Transformation
   Action]

  [TFunc
   (transformation (Id) Expr)
   (pred (Id) Expr)]

  [AFunc
   (lambda (Id Id) Expr)]

  [FilePath
   String]

  [Datashell
   (mk-datashell [Listof Any])
   (mk-datashell FilePath)
   Transformation]

  [Transformation
   (ds-map TFunc DataShell)]

  [Action
   (ds-reduce AFunc Expr Datashell)
   (ds-collect Datashell)])


@section{Creating A Datashell}

A @bold{Datashell} is an immutable structure that stores user inputted lists of data,
on which RSL Operations are performed.

You will able to create a Datashell with the following functions:


@defform[(mk-datashell l)
         #:contracts([l list?])]{
 Creates a Datashell from the given list.
}

Example:
@racketblock[(save-ds a (mk-datashell '(5 2)))]

@para{@bold{Feature/Bug:} Currently, the user is only able to call @bold{mk-datashell} within @bold{save-ds}.}

@defform[(mk-datashell path)
         #:contracts([path String?])]{
 Creates a Datashell from the given csv file path.
}

Example:
@racketblock[(save-ds a (mk-datashell "../path-to-file.csv"))]

@defform[(save-ds id (ds-map func datashell))
         #:contracts([l list?])]{
 Creates a Datashell from an existing Datashell.
 Note that @bold{ds-map} can only be used with @bold{save-ds}.
 @bold{ds-map} will be introduced in the @bold{Transformation} sections.
}

Example:
@racketblock[
 (define-map-func (sub-8 z)
   (- z 8))

 (save-ds b (ds-map sub-8 a))]


@para{Additionally, all Transformations return one and only one new Datashell.}

@section{Transformations}

@subsection{Defining Transformation Functions}

Transformations are @bold{lazily} evaluated.

A Transformation function provides a mapping from one item to another item.

@defform[(define-map-func (name arg) body)
         #:contracts([name id?]
                     [arg expr?])]{
 Creates a transformation function which can be passed into @bold{ds-map}.
 Note that @bold{define-map-func} only takes one input argument and produces one output.
}

Example:
@racketblock[
(define-map-func (sub-8 z)
  (- z 8))
]

@defform[(define-filter-pred (name arg) body)
         #:contracts([name id?]
                     [arg expr?])]{
 Creates a predicate which can be passed into @bold{ds-map} which when executed will filter out all
                                              items which do not pass the predicate.
 Note that @bold{define-filter-pred} only takes one input argument and produces a boolean.
}

Example:
@racketblock[
(define-filter-pred (less-than-5? num)
  (< num 5))
]

@subsection{Mapping Transformation Functions}

When called on a Datashell, transformations are queued up for later execution.
These transformations will not execute until an Action is called on the Datashell. This allow RSL to evaluate all transformations applied to the Datashell in one iteration.

@defform[(ds-map tfunc datashell)
         #:contracts(
                     [tfunc (or id? lambda?)]
                     [datashell Datashell?])]{
 Creates a new Datashell using the given Datashell mapped with the given function.
 The given procedure is not evaluted until an action is introduced.
}
@para{@bold{Feature/Bug:} The user is only able to call @bold{ds-map} within @bold{save-ds}.}

Example:
@racketblock[
 (save-ds a (mk-datashell '(5 2)))
 (save-ds ab (ds-map add-5 a))
]

@section{Actions}

Actions are @bold{eagerly} evaluated.

An Action immediatly triggers the evaluation of the datashell's queued transformation(s), and it reduces the transformed dataset into a value to return
to the user.

@defform[(ds-reduce afunc base datashell)
         #:contracts([afunc procedure?]
                     [base Any?]
                     [datashell Datashell?])]{
 Applies the datashell's queued transformations then immediatly evaluates and returns the result of the reduction function on the datashell.
 
 The return type of @bold{ds-reduce} can be any except a Datashell.
 The return type of ds-reduce is the same as the type of @bold{base}, and the return type of @bold{afunc}.
}

Example:
@racketblock[
 (save-ds a (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))
 (ds-reduce + 0 a)
]

@defform[(ds-collect datashell)
         #:contracts([datashell Datashell?])]{
 Returns a list from the given datashell by applying all the datashell's queued functions and immediatly collecting the results.
 
 @bold{ds-collect} returns a Listof Any.
}

Example:
@racketblock[
 (save-ds a2 (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))
 (save-ds ab2 (ds-map less-than-5? a2)) 
 (save-ds abc2 (ds-map mult-10 ab2))
 (save-ds abcd2 (ds-map multiple-of-20? abc2))

 (ds-collect abcd2)
 > (list 20 40)
]
The example above will return all functions applied above: 
