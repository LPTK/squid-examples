Squid Examples
===============

This repository contains example usages of the [Squid](https://github.com/epfldata/squid)) metaprogramming framework.

Currently, the only example here is:
 * the [Relation DSL](relation-dsl) tutorial: using quasiquotes to define transformations for compiling relational algebra queries to a push-based query engine. We optimize queries based on Schema information, specializing the data-structures used, and converting row layout to columnar layout. 
 
More to come soon. Some other examples, such as the streams library optimizer, have not yet been ported from [the _examples_ folder](https://github.com/epfldata/squid/tree/master/example/src) of the main Squid repository.
