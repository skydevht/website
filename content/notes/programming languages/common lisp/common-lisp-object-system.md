---
title: Common Lisp Object System (CLOS)
url: clos
publishdate: 2022-11-22
tags: [common lisp]
---

I’ve encountered Object-Oriented Programming when I was learning Python (which
is my first language). I’ve struggled with it because the paradigm is not as
obvious as using functions and structures in C. It finally clicked for me when I
was doing Android development in Java. Instead of instructions like in
imperative programming (and modularizing it with functions), your program is a
set of entities talking to each other and collectively achieving the required
goal.

I’ve been amazed by the CLOS. When doing OOP with Java, it always feels overly
complicated. An IDE is almost necessary to implement your design speedily. CLOS
have the leanness of C structures while implementing inheritance and
polymorphism so effectively it became transparent. I don’t know the exact
implementation details of CLOS, but I feel that it can be easily replicated in C
by adding the needed metadata in the structure and doing the needed type
checking in the functions.

The elegance of the CLOS has one surprising consequence: After learning it, it
is immediately obvious that in most cases, you don’t need it. The primitive
types like lists, hash maps, arrays, … are sufficient for most code, but it’s
there when OOP is the way to realize a neater implementation of your specs.
