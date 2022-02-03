---
title: My React Native Template

---

I've been working with React Native for about 3 years now. The projects have been applications mainly for small businesses and entrepreneurs as a freelancer during this time. After a few of these, I find myself using  the same architecture again and again.

Laravel's framework principles, together with “Clean architecture” from Robert C. Martin, have greatly influenced me. One of the core principles I follow is loose coupling. As I usually work alone, it allows me to effortlessly substitute modules I've implemented for another one.

# Main files

The most important files for me are the following:
- `index.js`,
- `App.js`,
- `config.js`,

These reside at the root of the project folder. I consider `index.js` as the loading logic. I rarely modify it from the default that is generated when creating the project using `react-native-cli`. 

`App.js` Is the super container. It is the starting point of the application, and it is where I integrate any React Native library that deals with context, like Theme, Navigation, and Redux. It is the only React files that depend on non-react modules.

I seldom use a config file. The rare cases I do is for holding constants in the business logic that can be modified later, or as a common place for service configurations.

# Navigation

`react-navigation` library has been enough for my needs so far. I usually configure it inside one file name `navigator.js`. It is the interface between the screens and the main App component. It does not handle navigation logic, just the configuration.

# Screens

I'm a big fan of the functional paradigm. Now I'm using functions for every React components I write. I think it is better for handling side effect from a readability standpoint. My preferred method is using files directly like `Screen.js` instead of `Screen/index.js` I've seen people do.   I add styling at the end of the file, and I use **flow** for specifying the type of the props. 

Screens are the main containers or smart components. That is where I write business logic and side effect code (like asynchronous code that deals with services). The components are the one dealing with the UI.

# Components

The majority the components are dumb. They display data or accept input from the user and hand out the result to the main container. They follow the same template as screens. I mainly write components in the same file that contains the screen that will use them (Row in a list). But I put reusable ones under the `components` subfolder of the project.

# Services
Services are for everything that interacts with the outside world. REST API, Baas, Persistent Storage, … all fall under that category. The key is to decouple the service from the main code. Apart from these files, there shouldn't be any mention of the services anywhere else — except `config.js`. They will contains the configuration logic that will be inside the file and a collection of functions to use elsewhere.

# Utility functions
These are small functions that contains logic you find yourself reusing throughout the app. Things like concatenating the first name and last name of the user, formatting strings, calculating various values... I just group them by category (`user.js`, `format.js`) and put them inside a `util` folder.

# State Management
This is an optional one. I mostly use Redux for caching API responses. It permits me to go from an asynchronous paradigm to a reactive one. I found myself not using it with Firebase and Apollo GraphQL, as the latter ones have their caching mechanism. When I do use Redux, I used the `rematch` library is it eliminates a lot of the boilerplate.

# Utility Libraries
The most notable examples are theming and localization libraries. They do not deal with the outside world but are important enough to not be relegated to the `util` folder. They are commonly stored alongside `App.js` as they will be integrated in it.

I did not bother setting up a repository for this. I predicted it will be the same amount of work as the components will likely change, and I will need to update the dependencies. This is a guideline that I follow to reduce the time spent on where I put each type of logic and focus on writing it.


