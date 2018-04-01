#lang scribble/manual

@; Main
@title[#:tag "Racket Spark Light Documentation" #:date "March 28, 2018"]{Racket Spark Light}
@author["Alex Cherry and Yifan Xing"]
@hyperlink["https://github.com/xingyif/racket-spark-light/tree/new-design"]{source code}
@section{Introduction}

Racket Spark Light (@bold{RSL}) is designed to efficiently transform data sets. RSL relies on the MapReduce model. The language provides an intelligent way of traversing large datasets by evaluating and optimizing operations used on the datasets. It will store all map transformations applied to a list instead of executing them immediately. The original dataset and the transformations that are applied to it are stored in a data structure called a Datashell. Evaluation of these transformations will not occur until a reduction action is called on the Datashell. The transformations' inputs, higher-order-function, are analyzed, reformed, and combined into one function when provided by user.

When the data stored in a Datashell is needed by the reduction action, RSL executes a single mapping operation. This operation is a combination of the previously stored transformations that only traverses the data once. Additionally, RSL will store the results of each transformed dataset in a graph. The graph provides fault-tolerant resiliency by storing the results of the operations in its nodes. This graph allows RSL to reuse the results of previous computations when performing future transformations. The central concepts of RSL are inspired by Apache Spark. Furthermore, RSL provides full access to the Racket language as well as to an interface that allows users to import data from CSV files into Datashells.


@section{Getting Started}
You will be able to find RSL from @hyperlink["https://github.com/xingyif/racket-spark-light/tree/new-design"]{racket-spark-light}.
To install RSL, simply clone the git repository.

To use RSL, simply type @bold{#lang s-exp "path/to/racket-spark-light.rkt"} at the top of the file.
With `s-exp`, you can use Racket’s default S-expres­sion reader with an arbi­trary expander.

@include-section["documentation.scrbl"]