---
title: Computation Models
tags: #theory_of_computation, #computer_science
created_on: 2022-07-19
updated_on: 2022-07-24
---

<div class="card draft">
  <p>This is still a draft</p>
</div>

One of the interest of the theory of computation is defining how problems can be computed. A model of computation defines our input and tells us the processing steps to attain a solution. We are not executing code on a processor. 

## Finite Automata

This is the simplest model. We have two main sets:
- our **alphabet**, consisting of *symbols*, a sequence of which will be our input,
- the **states** of the machine, each representing a distinct possibility our model can express.

From the latter, we have a subset, which will be our **accept states**. If the input ends and we are in one of these states. That means that we have found a solution for the problem that our machine have been designed for.

The main core of this model — and the others — are the transition function. This is the expression of each steps of our computation. The variables for this function are the next symbol of our input and the current state of the model and it tells us the next state. We also define one of the state as our *start state*, meaning the initial step is always taken from it.

### Regular Languages and Regular Expressions

The sets of input that always ends at an accept state is called a regular language of that machine. In other words, that the machine recognizes that language. This set supports a few operations:
- Union of two regular languages;
- Concatenation of two regular languages;
- Repetition of an elements of a regular languages, including zero times;

The resulting sets will be regular languages and therefore exists a machine that can recognizes them.

From there we can create specific machines that will recognize:
- A single symbol;
- The empty element
- Nothing

And use the above operation to create more complex languages

### Non-determinism

Our above machine can be more powerful if we allows parallel computation by allowing the transition function to return a set of states instead of a single one. We will say that our input is accepted if one of those branches results in an accept states. A specific symbols, called the empty string, will be added to our alphabet. It means that a transition can occur without reading a symbol from the input string.

## Pushdown Automata

### Context-Free Grammar

## Turing Machine