---
title: The usage of React Hooks

---

Functional components were a part of React before the introduction of Hooks. But their usage was restricted to creating pure components, as they did not possess the more complex lifecycle and state management of a class component. Hooks add these to functional components and allow us an easier way to reuse functionalities.

What are hooks? Hooks are functions. And like all functions, you provide them with arguments, and they return values. To understand the usage of hooks is to understand where your arguments and the returned values fit into your component usage.

Let's start with the basic: the function component. It accepts props as parameters and return a component tree. You can take the following as an example.

```javascript
const Message = (props) => {
  return <p>{props.text}</p>
}
```

A bare component should be pure. If the `props` object does not change — by altering the `text` prop —, neither should the returned component tree.  The lifecycle is the stages a component goes through from creation to deletion. For a functional one, the lifecycle is another execution of the component’s code. Mutating the props can trigger this new execution.

What if you want to change the color of the text when the user clicks on it? You could add a new prop called `color`, but the parent component will then be responsible for updating this new prop. The consequence is a tight coupling — one depends on the other — between the two components. The parent will have the code to declare and update the prop, while our component is the one using it and is responsible for triggering the change. To resolve the situation, we shall use the `state` concept.
 
To explain state, we can take water as an analogy. Water can have multiple aspects: liquid, vapor, and ice. Which all depends on the same measure inherent to the water — temperature. In other words, the temperature of the water determines the current state of the water. And if we know the current value of the temperature, it’s easy to know its aspect. Like temperature, our above component can have a variable called `color` which will always be the current color of the text. But, this has its limitations.
 
If we create the variable inside the function of the component, it will be deleted when the function returns. And props are currently the only way we're able to update the component. This is where **useState** comes in. ** useState** will provide you with a value that will not be destroyed when the function ends and, when changed, will trigger an update for the component — the function of the component will be executed again. **useState** returns an array: The first element is our value, the second is the function to update the value. Assigning a new value directly will not work. This is our updated component:
 
 ```javascript
 const Message = (props) => {
	const [color, setColor] = useState('blue');
	const handleClick = () => {
		setColor(color === 'blue' ? 'red' : 'blue');
	}
	return (
		<p style={{color: color}} onClick={handleClick}>
			{props.text}
		</p>
	);
}
 ```
 
Note that I set an initial value for the color. A rough overview on what happens under the hood:

* For the initial execution of our code, we called **useState** with the initial value we want. It returns this value, and we store it in `color`. The second element is the update function, and we store it in `setColor`. React guarantees that the function will never change.
* On the subsequent runs of the code, useState returns the same value. If the update function was called, the modified value will now be the one returned. The initial value will no longer be used.
* If the component is unmounted — removed from the webpage —, we go back to step one on the next mounting.
 
Now, every time we click on our message, its color will alternate between blue and red. This feature is self contained, making the component reusable. But right now, our state  changes only when we act on it. Why not make it evolve by itself? Let’s make it update the color when the text changes.
 
Remember that our component started pure, always returning the same tree when provided with the same props. We added state, but to manipulate it, we need an external event. To link state and props together, and to react to changes to both, we need a way to detect when they’ve been modified. And that’s what **useEffect** give us. With **useEffect**, you can have a piece of logic that will run once the component is mounted — created and added to the web page — and when any element of a provided set of state variables and props — the dependencies — is updated. For our component, we have a unique element to observe — the text prop. Here is the new code:
 
```javascript
const Message = ({ text }) => {
	const [color, setColor] = useState(null);
	useEffect(() => {
		if (color == null) setColor('blue');
	  else setColor(color === 'blue' ? 'red' : 'blue');
	}, [text])
	return (
		<p style={{color: color == null ? 'blue' : color}}>
			{text}
		</p>
	);
}
```
 
That’s when it’s got tricky. We now have multiple stages:
 
 * The component is created, returning an initial component tree and registering our effect.
 * Our effect is run once for the creation of the component.
 * Then it will run for every change to its dependency.

That’s why we started with `null` as the value of the `color` state. The main logic is to alternate between two colors, we need a third value to detect when  it’s the initial run. In the component tree, we then alleviate for this third value, as it’s not a correct value for the specifications of our component. A special note: If you return a function inside your effect, it will be executed when the component is destroyed. And if the dependencies is an empty set, the effect will be executed only once, right after the component is created, which is useful for initialization.

With these two hooks, you can replicate the majority of the features that were only possible with class component. Another two hooks I find useful are **useRef** and **useMemo**.

**useRef** comes in when you want to store a value after the code of the component is executed for the next execution, but you don’t want its mutation to trigger a new execution. It acts like a global variable in respect to the component. If we take the following code:

```javascript
const Message = ({ text }) => {
	const interval = useRef(null)
	const [color, setColor] = useState('blue');
	useEffect(() => {
		interval.current = setInterval(() => {
			setColor(color === 'blue' ? 'red' : 'blue')
		}, 1000);
	  return () => {
		  if (interval.current) clearInterval(interval.current);
	  }
	}, [])
	return (
		<p style={{color: color == null ? 'blue' : color}}>
			{text}
		</p>
	);
}
```

The component is now flashing between blue and red every second. When we unmount the component, we need to remove the interval to stop `setColor` being called. The long-lived aspect that **useRef** provides is useful in that case. Notice that we don’t update the object returned by **useRef**, but its property `current`. We removed the `text` prop from the dependencies set, as our effect role is to initialize the interval. Additionally, it returns a function to be executed when the component is unmounted, clearing the interval.

**useMemo** is for improving the performance of our code. Sometimes, we have to do computation on our state and props, resulting in a new value. If we add the code to the body of our component, it will be run each update. **useMemo** allows us to run the computation when the dependencies set changes and not on every render. Let’s take a look at an example:

```javascript
const Message = ({ text }) => {
	const [color, setColor] = useState(null);
	useEffect(() => {
		if (color == null) setColor('blue');
	  else setColor(color === 'blue' ? 'red' : 'blue');
	}, [text])
	const bgColor = useMemo(() => getInvertedColor(color), [color])
	return (
		<p style={{color: color == null ? 'blue' : color, background: bgColor}}>
			{text}
		</p>
	);
}
```

`getInvertedColor` is our heavy computation code. **useMemo**, takes a function and a dependencies array. The body of that function should be statements that we could have put inside the body of the component, and should follow the same pure paradigm — no side effect. The return value is returned directly by useMemo. The function executes on the mounting stage and when the dependencies update. But the return value will be stored — memoized — and return directly otherwise. We can mention the `useCallback` hook, which memoizes a function instead.  

The most important part is that you can refactor the above code to create your own hook, making it possible to share functionalities between components.

```javascript
const useColorFromText = (text) => {
 const [color, setColor] = useState(null);
	useEffect(() => {
		if (color == null) setColor('blue');
	  else setColor(color === 'blue' ? 'red' : 'blue');
	}, [text])
	return color == null ? 'blue' : color;
}
const Message = ({ text }) => {
	const color = useColorFromText(text);
	return (
		<p style={{color: color}}>
			{text}
		</p>
	);
}
```
