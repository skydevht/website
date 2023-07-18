---
title: Logic Programming with Prolog
url: prolog
publishdate: 2023-01-06
tags: [prolog]
---
My first programming languages was C, which is part of the Fortran family. Lately, I've been learning fundamentally different languages, starting with Lisp and the more modern Clojure. I've taken a brief look at Forth. The latest I've tried is Prolog. And it was by far the most enlightening experience I had.

Prolog is a logic programming language, allowing you to express facts and rules about a given domain. Facts are rules or relations that you hold as true. Rules can contain other rules and itself. It is true if all the sub-rules are true. A query is a fact evaluated by the interpreter, either verifying if it's true or what values will make it true.

Expressing a program in terms of relations instead of statements is great for prototyping. Rather than finding a computation procedure, you're describing all the known facts of the problem, then its constraints. A Prolog algorithm is more a specification than a procedure. The only pain point for me now is the difficulty of dealing with side effects, mainly IO.
