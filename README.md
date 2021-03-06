Squid Examples
===============

This repository contains example usages of the [Squid](https://github.com/epfldata/squid)) metaprogramming framework.

Currently, the only example here is:
 * the [Relation DSL](relation-dsl) tutorial: using quasiquotes to define transformations for compiling relational algebra queries to a push-based query engine. We optimize queries based on Schema information, specializing the data-structures used, and converting row layout to columnar layout. 
 
More to come soon. Some other examples, such as [the streams library optimizer](https://github.com/epfldata/squid/tree/ae115c9385f7d2a1da3c4a0e87e11670b9116d88/example/src/main/scala/experimentation/sfusion2), have not yet been ported from [the _examples_ folder](https://github.com/epfldata/squid/tree/master/example/src) of the main Squid repository.
