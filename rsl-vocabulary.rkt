#lang racket

#|

-----------RSL Core Vocabularies-----------:

A Data is a collection of elements, on which operations are performed. And it is provided by the user and stored in a DataShell.


A DataShell is an immutable structure that stores user input Data, on which DSOperation are performed.
The user is able to create a DataShell by:
 1. Importing existing Data into RSL via the data-importing interface
 2. Applying a Transformation on an existing DataShell
The user is not able to modify any existing DataShell, or apply any non-DSOperations on DataShell.


An DSOperation is one of:
- Transformation
- Action


A Transformation creates a new DataShell from the existing DataShell.
All Transformations are lazy, which means that they are not evaluated when the user calls them.
Rather, RSL records a graph of transformations applied on the base DataShell (provided by the user).
The Transformations are only analyzed, optimized, and computed when an Action is applied on the same DataShell.
Using lazy Transformations allow RSL to evaluate and optimize the operations applied on the DataShell,
and traverse the list as few times as possible. Therefore, it improves the performance of processing large data sets.

RSL supported Tranformations include:
- map(func):
     | Returns a new DataShell formed by passing each element of the original DataShell through the input function.

- filter(func):
     | Returns a new DataShell formed by selecting the elements from the original DataShell that return true when apply the given predicate.

- flatmap(func):
     | Similar to map, but the input function can return one element or a collection of the elements.



An Action triggers the evaluations of the transformation(s) and returns a value to the user.
Actions are Eager operations that are evaluated immediately.
RSL supported Actions include:
- reduce(func):
    | Folds the elements of the DataShell using the given function (λ (x y) z).

If time permits:
- collect():
    | Returns all elements from the DataShell as a list.
- count():
    | Returns the total number of elements in the DataShell

The user is able to use any of the supported DSOperations on DataShells, however, the user is not allowed
to determine whether an operation is a Transformation or an Action.


A Lazy Evaluation is a strategy that purposefully delays the evaluation of the given function until the
value is needed. RSL intelligently uses laziness to improve the performance of processing large data sets.

An Eager Evaluation is a strategy that evaluates the given function as soon as it is provided.
It is the opposite of a Lazy Evaluation. In RSL, Eager Evaluations trigger Lazy Evaluations.



A Higher-order function is a function that takes another function as an argument.


In addition:
The user has access to all Racket types and expressions.
A number is one of: 1, -1, 3/5, 1.22, #i1.22, 0+1i, and so on.
A Boolean is one of: #true or #false.
A string is one of: "", "he says \"hello world\" to you", "doll", and so on.
In general, it is a sequence of characters enclosed by a pair of ".
Etc.


|#
