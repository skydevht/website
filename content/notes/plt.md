---
title: Programming Language Theory
slug: plt
published_date: 2022-11-25
tags: [thinking, plt]
---

One of the things I’m noticing while studying programming languages is the notion of formal system. My current understanding is that there are two major principles. First, there are no real objects for the operations described in the system, meanings after doing the initial mapping to symbols – which the operations manipulate – we can discard the underlying entities. Second, the main mechanism is a collection of rules – logical methods of inference – that manipulates sequences of symbols to generate new arrangements. 

The most definitive example I have now is context-free grammar. There are two kinds of symbols: variables and terminals. The rules relate a single variable to a string of variables of terminates, and means that the former can be replaced with the latter. If we define a variable as the root, we will have a chain of transformations until we are left with only the terminals. The collection of all the resulting strings is called a language.

Programming languages embodies the same transformations. When we are coding, we are writing strings of characters – terminal symbols – that need to follow some order to fit the definition of the language. A programming language can be seen as the set of all possible programs that can be coded with it. And the rules are described in the grammar of the languages, like the one for [ANSI C](https://www.lysator.liu.se/c/ANSI-C-grammar-y.html) or [Python](https://docs.python.org/3/reference/grammar.html).

The main use of a program is to describe the evolution of an abstract entity called *computational process*. Which in turn manipulate another types of abstract entities called *data*. These abstract entities are the ones we cast aside when we start describing our formal system, but they retain their association even through the successive transformation that the system implement. Both the process and the data lives inside what we called an *abstract machine* and are constrained by its capabilities. 

Mapping the relation between the rules of the programming languages and the process running inside the machine is the domain of semantics. Which describes the possible evolution of the process described in the program and try to determine if the final state is part of the set of correct results. One of the advantage of the formal system is allowing us to construct new abstractions that enable easier reasoning about the objects (data and capabilities of the abstract machine). But, the formal system isn't concerned with the nature of things; thus an operation can be valid within the system, but results in error when applied to the underlying objects. One way to improve correctness is adding a type system to the programming language.
ure of things, thus an operation can be valid within the system, but results in error when applied to the underlying objects. One way to improve correctness is adding a type system to the programming language.
