---
title: Loop in Common LISP
tag: #common_lisp
created: 2022-11-04
updated: 2022-11-04
---

# The loop macro

The `loop` macro introduce a special [Domain-specific language](https://en.wikipedia.org/wiki/Domain-specific_language) for iteration consisting of multiple clauses. These clauses can be classified into multiple categories:

* Variables initialization:
    * Only once with `with`,
    * While iterating with `for|as`,
* Forms evaluation before the iteration with `initially`,
* Conditional clauses for termination,
* Clauses for evaluating forms, with some having specific actions,
* Forms evaluation after the iteration with `finally`

There are great libraries for doing iterations like [SERIES](https://series.sourceforge.net/) and [iterate](https://iterate.common-lisp.dev/). In the case of the latter, its developers make a good case on why you should prefer `iterate` over `loop`.

# The do* macros

`dotimes` is great if you need the index when doing the iteration, such as when `setf`-ing. `dolist` can be shorter and more expressive than its `loop` counterpart. I haven’t use `do` yet.


