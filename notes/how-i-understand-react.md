---
title: How I understand React
tags: #react_native, #javascript, #programming
created_on: 2022-05-02
updated_on: 2022-07-21
---

Every time I learn a new language, library or framework, I like to form a mental image of how it works and why it works that way. It helps tremendously with being able to provide practical solutions to business problems if you can discern their pros and cons easily.

I don't learn the technical aspects by heart. You will retain them overtime by practicing regularly with the tool. Instead, I focus on the architectural level and I try to understand how everything fits together.

As for React, I think there are three major aspects that one needs to understand well to be proficient with it. They are the virtual DOM, the components and the component's lifecycle.

# The virtual DOM

The DOM represents a document with a logical tree structure. Almost every UI library is represented with a tree structure because it helps with geometrical transformations and property inheritance. React's virtual DOM is a copy of that structure. Because modifying the real DOM is costly — computing the new UI representation takes time —, React executes the manipulation first on its copy, then compare the new and the old versions to determine the most performance effective way to update the real DOM.

That means that what you are writing and updating is not the browser’s DOM. It does not have the same properties, and you should not treat it the same way. That also means that the React philosophy is universal, as the DOM in the browser is similar in representation to the UI library in other platforms (Which explains React Native). It is a tree structure, but with new types of nodes. Here is [a post](https://programmingwithmosh.com/react/react-virtual-dom-explained/) that explains the virtual DOM in more detail.

# The components

The components are each responsible for a section of the virtual DOM, which may contain other components. There are two types of components: classes, and functions. I like the latter as it's easier to manipulate. With hooks, you can use a function instead of a class, as I do now.

As a developer, what you will be doing is creating a tree of components that will include your owns and those provided by libraries. These components will accept props as inputs and will return the section of the tree they are responsible for. There are special components called HOC (Higher-Order Components) which are functions that will return either your component with new additional props or a new component which will include your component as a child.

# The component lifecycle

Where does the business logic fit? In the case of a class component, there are various stages and `React.Component` class provides you with methods that will be called at each one of the stages. You choose the correct stage based on what you want to do. A few are called once upon the creation (mounting) and the destruction (unmounting) of your component, others will be called many times when your component updates (triggered by many things). Here is a [more detailed explanation](https://programmingwithmosh.com/javascript/react-lifecycle-methods/).

Using functions make things cleaner. There is no more lifecycle to worry about. You are provided with the props as parameters, and you need to return the tree's section. Now with hooks, you can do the same thing that the class component used to do. Hooks are functions that will accept arguments based on the current props and optionally will return objects that can be used inside the functions. Those objects are not created inside the function scope, so they won't be destroyed when the function returns. I think they get destroyed when the section of the virtual DOM your component is responsible for is destroyed.

That is my current mental model of React. There are many other concepts like the relation between refs and the DOM, Babel, and JSX. But I think these three are the most important concepts you need to wrap your head around.