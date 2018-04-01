#lang scribble/manual


@title{Racket Spark Light Documentation}

This section will expain each available feature in RSL.


@section{Grammer}

@(racketgrammar*

  [program
   (tempo n) top-expr ...]

  [top-expr
   (define id expr)
   (play player-expr)])


@section{Creating A Datashell}

A DataShell is an immutable structure that stores user inputted lists of data,
on which DSOperation are performed.

You will able to create a DataShell by:
 
 1. Creating a DataShell directly from a list
@defform[(mk-datashell l)
         #:contracts([l list?])]{
 Creates a Datashell from the given list.
}
Example:
@codeblock{(save-ds a (mk-datashell '(5 2)))}

2. Importing existing Data into RSL via the data-importing interfaces
@defform[(mk-datashell path)
         #:contracts([path String?])]{
 Creates a Datashell from the given csv file path.
}
Example:
@codeblock{(save-ds a (mk-datashell "../path-to-file.csv"))}

 3. Applying a Transformation on an existing DataShell
@defform[(save-ds (ds-map func datashell))
         #:contracts([l list?])]{
 Creates a Datashell from an existing Datashell.
 Note that @bold{ds-map} can only be used with @bold{save-ds}.
 @bold{ds-map} will be introduced in the @bold{Transformation} sections.
}
Example:
@codeblock{
 (define-map-func (sub-8 z)
   (- z 8))

 (save-ds b (ds-map sub-8 a))}


@larger{You are not able to apply any non-rsl operations on DataShell.
Datashells cannot be mutated, all DSOperations return a new Datashell.}



@section{RSL Operations}

@subsection{Transformations}

A Transformation creates a new DataShell from the existing DataShell.
All Transformations are lazy, which means that they are not evaluated when the user calls them.
Rather, RSL records a graph of transformations applied on the base DataShell (provided by the user).
The Transformations are only analyzed, optimized, and computed when an Action is applied on the same DataShell.
Using lazy Transformations allow RSL to evaluate and optimize the operations applied on the DataShell,
and traverse the list as few times as possible. Therefore, it improves the performance of processing large data sets.

@bold{RSL supported transformation operations include:}

@defform[(define-map-func (name arg))
         #:contracts([name id?]
                     [arg expr?])]{
 Creates a transformation function which can be passed into @bold{ds-map}
 Note that @bold{define-map-func} can only have one input argument and produce one output.
}

Example:
@codeblock{
(define-map-func (sub-8 z)
  (- z 8))
}

@defform[(define-filter-pred (name arg))
         #:contracts([name id?]
                     [arg expr?])]{
 Creates a transformation function which can be passed into @bold{ds-filter}
 Note that @bold{define-filter-pred} can only have one input argument and produce a boolean.
}

Example:
@codeblock{
(define-filter-pred (less-than-5? num)
  (< num 5))
}


@bold{RSL supported Tranformations include:}

@defform[(ds-map tfunc datashell)
         #:contracts(
                     [tfunc (or id? lambda?)]
                     [datashell Datashell?])]{
 Creates a new Datashell using the given Datashell mapped with the given function.
 Note that @bold{ds-map} is a transformation, which evaluates the operation lazily.
 The given operation is not evaluted until an action is introduced. Therefore, the user is only able to call @bold{ds-map} with @bold{save-ds}
}

Example:
@codeblock{
 (save-ds a (mk-datashell '(5 2)))
 (save-ds ab (ds-map add-5 a))
}


@defform[(ds-filter tfunc datashell)
         #:contracts(
                     [tfunc (or id? lambda?)]
                     [datashell Datashell?])]{
 Creates a new Datashell using the given Datashell mapped with the given predicate.
 Note that @bold{ds-filter} is a transformation, which evaluates the operation lazily.
 The given operation is not evaluted until an action is introduced. Therefore, the user is only able to call @bold{ds-filter} with @bold{save-ds}
}

Example:
@codeblock{
 (save-ds a (mk-datashell '(5 2)))
 (save-ds ab (ds-filter less-than-5? a))
}


@subsection{Actions}
An Action triggers the evaluations of the transformation(s) and returns a value to the user.
Actions are Eager operations that are evaluated immediately.
@bold{RSL supported Actions include:}
@defform[(ds-reduce afunc base datashell)
         #:contracts(
                     [afunc (or id? lambda?)]
                     [base any?]
                     [datashell Datashell?])]{
 Applies the given function(action) to the given datashell.
 Note that @bold{ds-reduce} is an action, which evaluates the operation eagerly.
 The given operation is composed with the previous composed transformations applied on the datashell, and the newly composed function is evaluted immediately on the given datashell.
 @bold{ds-reduce} can return anything except a datashell.
}

Example:
@codeblock{
 (save-ds a (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))
 (ds-reduce + 0 a)
}

- ds-collect():
    | Returns all elements from the DataShell as a list.

@defform[(ds-collect datashell)
         #:contracts([datashell Datashell?])]{
 Returns a list from the given datashell by applying all the composed functions on the original datashellthe, and collects the results.
 Note that @bold{ds-collect} is an action, which evaluates the operation eagerly.
 @bold{ds-collect} can return anything except a datashell.
}

Example:
@codeblock{
 (save-ds a2 (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))
 (save-ds ab2 (ds-map less-than-5? a2)) 
 (save-ds abc2 (ds-map mult-10 ab2))
 (save-ds abcd2 (ds-map multiple-of-20? abc2))

 (ds-collect abcd2)
}
The example above will return all functions applied above: => (list 20 40)
