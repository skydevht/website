---
title: The React library
slug: react
published_date: 2023-06-17
tags: [react, ui, webdev]
---

# The React component
Most applications are built on top of other systems, and their execution is confined by those systems. Similarly, a web application is built on top of the JS runtime inside the browser and the DOM API provided to manipulate the rendered webpage. React is a library and a paradigm designed for easier manipulation of the DOM, especially for web applications that reuse common sets of widgets.

React allows us to encapsulate parts of the UI as components. These can be included inside other components, forming a tree structure similar to the DOM but simpler. This representation can be rendered to a tree of HTML elements—or other native elements, depending on the platform—by a recursive process. The selling point of React is that this compilation happens in memory without touching the real DOM (an expensive process); instead, it computes the difference between this virtual DOM and the real one, then only applies the changes. This is the reconciliation process.

Creating a React application means structuring your UI as a tree of components. Each component accepts a set of properties called `props` – and then returns the subset of the tree it's responsible for. The leaf nodes are obviously the native elements of the platform, which are provided by the React library and ready to be used in this paradigm. React allows us to write logic to compute the subset of the tree being rendered, which is mainly dependent on the `props`, always provided by the direct parent of the component.

React guarantees that when one of the `props` is mutated, our render logic will run, and we can "react" to the change, making sure that the subtree is properly updated. This results in a simpler mechanism of change propagation for our UI.
