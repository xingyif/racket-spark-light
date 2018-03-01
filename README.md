# racket-spark-light (RSL)

**What is the purpose of RSL?**

Our DSL is designed to transform and analyze very large data sets. It focuses on using the MapReduce model, specifically optimizing map transformations as part of the language. By preprocessing these transformations, we can avoid needlessly traversing the list for every single transformation. The language provides an intelligent way of traversing large datasets by evaluating and optimizing operations used on the datasets.

Using a central idea from Apache Spark, all list transformations are evaluated lazily, combining all maps into a single mapping at the moment of reduction that performs all queued up transformations. Evaluation of these transformation does not occur until an “action” or “reduction” is applied upon them. An action is some computation upon a list where the result is needed immediately for use. We will provide an interface for reading in CSV files in order to import real data. If a user doesn’t use our data types and provided procedures, they will be able to use Racket exactly how they can with #lang racket.
