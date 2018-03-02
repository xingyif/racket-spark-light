#lang racket

#|

-----------RSL Vocabulary/Keywords-----------:


-----DataShell-----:
A DataShell is an immutable structure that stores user inputted lists of data,
on which DSOperation are performed.
The user is able to create a DataShell by:
 1. Importing existing Data into RSL via the data-importing interfaces
 2. Creating a DataShell directly from a list
 3. Applying a Transformation on an existing DataShell

The user is not able to apply any non-DSOperations on DataShell.
Datashells cannot be mutated, all DSOperations return a new Datashell.


-----DSOperation-----:
An DSOperation is one of:
- Transformation
- Action


-----Transformation-----:
A Transformation creates a new DataShell from the existing DataShell.
All Transformations are lazy, which means that they are not evaluated when the user calls them.
Rather, RSL records a graph of transformations applied on the base DataShell (provided by the user).
The Transformations are only analyzed, optimized, and computed when an Action is applied on the same DataShell.
Using lazy Transformations allow RSL to evaluate and optimize the operations applied on the DataShell,
and traverse the list as few times as possible. Therefore, it improves the performance of processing large data sets.

RSL supported Tranformations include:
- ds-map(func):
     | Returns a new DataShell formed by passing each element of the original DataShell through the input function.

- ds-filter(func):
     | Returns a new DataShell formed by selecting the elements from the original DataShell that return true when apply the given predicate.

- ds-flatmap(func):
     | Similar to map, but the input function can return one element or a collection of the elements.


-----Action-----:
An Action triggers the evaluations of the transformation(s) and returns a value to the user.
Actions are Eager operations that are evaluated immediately.
RSL supported Actions include:
- ds-reduce(func):
    | Folds the elements of the DataShell using the given function (λ (x y) z).

If time permits:
- ds-collect():
    | Returns all elements from the DataShell as a list.
- ds-count():
    | Returns the total number of elements in the DataShell


-----What user can/cannot do-----:
The user is able to use any of the supported DSOperations on DataShells, however, the user is not allowed
to determine whether an operation is a Transformation or an Action.


-----In addition-----:
The user has access to all Racket types and expressions.

|#
