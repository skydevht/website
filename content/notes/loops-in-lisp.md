---
title: Loops in Common Lisp
slug: loops-in-lisp
type: notes
published_date: 2022-11-15
tags: [common lisp]
---

The `loop` macro introduce a special [Domain-specific language](https://en.wikipedia.org/wiki/Domain-specific_language) for iteration consisting of multiple clauses. These clauses can be classified into multiple patterns

* Variables initialization:
	* Once with `with`,
	* While iterating with `for|as`,
* Forms evaluation before the iteration with `initially`,
* Conditional clauses for termination,
* Clauses for evaluating forms with some having specific actions,
* Forms evaluation after the iteration with `finally`

There are great libraries for doing iterations like [SERIES](https://series.sourceforge.net/) and [iterate](https://iterate.common-lisp.dev/)
