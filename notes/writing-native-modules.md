___
title: Writing Native Modules
tags: #programming, #react, #ionic
___

One of the milestones of being a React Native developer is writing your first native module. At first, it may appear a daunting task, but the module part is easy to wrap your head around. Here are a few important points to keep in mind. The same applies to Cordova.

# The Bridge

The bridge concept is not a unique to React Native. It is a common occurrence when you’re dealing with many languages or multiples platforms. It is how two different layers in a single system communicate with each other.

The main logic of the application resides usually in one of the layer, while the feature will be implemented in the other layer. The first will be the caller and the second will be the callee. For the two to communicate, there should be a common ground between the two, which we can call the host.

The host can be the same as one of the layers. In game engines, which often need to add a scripting layer, the host is the caller and the callee will be the scripts that are written to customize components’ behaviors. In our case (React Native and Cordova) the host is the callee. Our code reside in the JavaScript interpreter (custom engine or web view).

As the host is the one running the layers, it is trivial for it to customize their engine or interpreter. It will provide new functions in addition to the standard ones, which will be how the layer will get back to the host. Those functions may be arranged in a more cohesive set for a more intuitive use. It will replicate the caller layer’s data types in the callee layer.

# A module is a plugin

Occasionally, the callee layer will need to execute before getting called, or even before the main layer is executed. In those cases, there will be a general framework to follow with hooks. The framework is to tell the host the functions to call, initialize or destroy the module. The hooks are there to add new features to the host without the other layer intervention (which cannot access these parts directly in our case). These hooks are why we normally need a module in the first place.

# A module is a library

But, how do we architect the module in the first place? From the host's perspective, it needs to be executable logic. While the host is initializing, it may run initialization code in the module. The same when the host is terminated. Logic in the module will mirror the host lifecycle. But from the perspective of the caller layer, it has to be a library.

Ordinarily, the module purpose is to provide a new feature to the caller layer. It needs to be architected the same way as the libraries native to the caller layer. Which is to provide a consistent set of functions (API) for the caller layer to use. At the callee side, we must mark those functions, and they have to follow the same signature (using the replicated data types).

# In conclusion

The above is my mental model when writing a native module. The implementation will differ based on the platforms, but it provides a quick way to pick the relevant parts to learn from the documentation and example codes.