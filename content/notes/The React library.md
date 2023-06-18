---
title: The React library
slug: react
published_date: 2023-06-17
tags: [react, ui, webdev]
---

All applications are built on top of other systems, and their execution is confined by those systems. Similarly, a web application is built on top of the JS runtime inside the browser and the DOM API provided to manipulate the rendered webpage. React is a library and a paradigm designed for easier manipulation of the DOM, especially for web applications that reuse common sets of widgets.

## The React component

React allows us to encapsulate parts of the UI as components. These can be included inside other components, forming a tree structure similar to the DOM but simpler. This representation can be rendered to a tree of HTML elements—or other native elements, depending on the platform—by a recursive process. The selling point of React is that this compilation happens in memory without touching the real DOM (an expensive process); instead, it computes the difference between this virtual DOM and the real one, then only applies the changes. This is the reconciliation process.

Creating a React application means structuring your UI as a tree of components. Each component accepts a set of properties called `props` – and then returns the subset of the tree it's responsible for. The leaf nodes are obviously the native elements of the platform, which are provided by the React library and ready to be used in this paradigm. React allows us to write logic to compute the subset of the tree being rendered, which is mainly dependent on the `props`, always provided by the direct parent of the component.

React guarantees that when one of the `props` is mutated, our render logic will run, and we can "react" to the change, making sure that the subtree is properly updated. This results in a simpler mechanism of change propagation for our UI.

## Hooks in React

In its simpler form, The React component is a pure function, accepting `props`  as arguments and returning a tree of components. But the user interface of a web application – or any application – is a state machine, transitioning from one state to another based on events and user actions. Pure functions are direct relationship between arguments and return values, thus no such transitions are possible.

To make a component – a pure function – stateful, React introduced hooks. These form an API that provides access to the – very stateful – system that executes the component-function. Data and code can then be stored, executed or retrieved inside the system. Hooks are a functional bridge between the function describing the behavior of the component and the system that is responsible for the reconciliation process. The latter is where the hooks function are implemented and executed. 

The simplest hook, IMO, is `useRef`. The first call of this hook, which happens when the component is first mounted, creates an object inside the system and then stores the initial value in the `current` property of that object. This object is then returned. Each subsequent call returns the same object until the component is destroyed, unmounted in the React's idiom.

The `useState` hook behaves similarly. But the object returned is an array with two elements. The first is a value, the other is a special function for updating this value and triggering another render. The argument passed is used when the hook is first called to set an initial value.

The `useEffect` hook is mostly to sync the React part of your application and another module like the REST service, a widget backend by another framework or even the native capabilities of the platform. To do so, you provide a function that enables the syncing and a list of reactive values, the `deps` array. The function provided is run when the component is mounted, i.e., attached to the tree that is bound to the real DOM, and when one of the elements in the `deps` array is mutated. The function may have a return value, which must be a function. This returned function will be executed before the next execution of the function provided to the hook.

The `deps` array plays  the same role in other hooks like `useCallback` and `useMemo`, it lets the execution system of the component lets when to carry out the hook operation. In the case of the `useCallback`, it's saving the new function and to compute a new value using the function provided to `useMemo` 

With the hooks, we can make the pure function – the specification of the component's behavior – become stateful in practice while retaining the same functional paradigm. The actual execution of the behavior, done by the React library, is where the magic happens.

## Custom Hooks

Custom hooks are simple. They are functions which encapsulate your previous usage of the [[Hooks in React]]. Thus, logic become reusable throughout your components. 

The direct usage can be: 
```js
const [data, setData] = useState()
const useEffect(() => {
	getData().then(res => setData(res));
}, [])
```

We can create a custom hook `useData` like this:
```js
function useData() {
	const [data, setData] = useState()
	const useEffect(() => {
		getData().then(res => setData(res));
	}, [])
	return data;
}
```

Its usage in the component would be:
```js
const data = useData()
```

Readability and maintainability is improved by refactoring to custom hooks.
