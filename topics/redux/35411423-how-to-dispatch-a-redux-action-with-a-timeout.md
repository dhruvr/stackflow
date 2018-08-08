
# How to dispatch a Redux action with a timeout?

## Question
      
I have an action that updates notification state of my application. Usually, this notification will be an error or info of some sort. I need to then dispatch another action after 5 seconds that will return the notification state to initial one, so no notification. The main reason behind this is to provide functionality where notifications disappear automatically after 5 seconds.

I had no luck with using `setTimeout` and returning another action and can't find how this is done online. So any advice is welcome.
## Answer
      
Don’t fall into the [trap of thinking a library should prescribe how to do everything](http://amasad.me/2016/01/03/overcoming-intuition-in-programming/). If you want to do something with a timeout in JavaScript, you need to use `setTimeout`. There is no reason why Redux actions should be any different.

Redux _does_ offer some alternative ways of dealing with asynchronous stuff, but you should only use those when you realize you are repeating too much code. Unless you have this problem, use what the language offers and go for the simplest solution.

Writing Async Code Inline
-------------------------

This is by far the simplest way. And there’s nothing specific to Redux here.

    store.dispatch({ type: 'SHOW_NOTIFICATION', text: 'You logged in.' })
    setTimeout(() => {
      store.dispatch({ type: 'HIDE_NOTIFICATION' })
    }, 5000)
    

Similarly, from inside a connected component:

    this.props.dispatch({ type: 'SHOW_NOTIFICATION', text: 'You logged in.' })
    setTimeout(() => {
      this.props.dispatch({ type: 'HIDE_NOTIFICATION' })
    }, 5000)
    

The only difference is that in a connected component you usually don’t have access to the store itself, but get either `dispatch()` or specific action creators injected as props. However this doesn’t make any difference for us.

If you don’t like making typos when dispatching the same actions from different components, you might want to extract action creators instead of dispatching action objects inline:

    // actions.js
    export function showNotification(text) {
      return { type: 'SHOW_NOTIFICATION', text }
    }
    export function hideNotification() {
      return { type: 'HIDE_NOTIFICATION' }
    }
    
    // component.js
    import { showNotification, hideNotification } from '../actions'
    
    this.props.dispatch(showNotification('You just logged in.'))
    setTimeout(() => {
      this.props.dispatch(hideNotification())
    }, 5000)
    

Or, if you have previously bound them with `connect()`:

    this.props.showNotification('You just logged in.')
    setTimeout(() => {
      this.props.hideNotification()
    }, 5000)
    

So far we have not used any middleware or other advanced concept.

Extracting Async Action Creator
-------------------------------

The approach above works fine in simple cases but you might find that it has a few problems:

*   It forces you to duplicate this logic anywhere you want to show a notification.
*   The notifications have no IDs so you’ll have a race condition if you show two notifications fast enough. When the first timeout finishes, it will dispatch `HIDE_NOTIFICATION`, erroneously hiding the second notification sooner than after the timeout.

To solve these problems, you would need to extract a function that centralizes the timeout logic and dispatches those two actions. It might look like this:

    // actions.js
    function showNotification(id, text) {
      return { type: 'SHOW_NOTIFICATION', id, text }
    }
    function hideNotification(id) {
      return { type: 'HIDE_NOTIFICATION', id }
    }
    
    let nextNotificationId = 0
    export function showNotificationWithTimeout(dispatch, text) {
      // Assigning IDs to notifications lets reducer ignore HIDE_NOTIFICATION
      // for the notification that is not currently visible.
      // Alternatively, we could store the interval ID and call
      // clearInterval(), but we’d still want to do it in a single place.
      const id = nextNotificationId++
      dispatch(showNotification(id, text))
    
      setTimeout(() => {
        dispatch(hideNotification(id))
      }, 5000)
    }
    

Now components can use `showNotificationWithTimeout` without duplicating this logic or having race conditions with different notifications:

    // component.js
    showNotificationWithTimeout(this.props.dispatch, 'You just logged in.')
    
    // otherComponent.js
    showNotificationWithTimeout(this.props.dispatch, 'You just logged out.')    
    

Why does `showNotificationWithTimeout()` accept `dispatch` as the first argument? Because it needs to dispatch actions to the store. Normally a component has access to `dispatch` but since we want an external function to take control over dispatching, we need to give it control over dispatching.

If you had a singleton store exported from some module, you could just import it and `dispatch` directly on it instead:

    // store.js
    export default createStore(reducer)
    
    // actions.js
    import store from './store'
    
    // ...
    
    let nextNotificationId = 0
    export function showNotificationWithTimeout(text) {
      const id = nextNotificationId++
      store.dispatch(showNotification(id, text))
    
      setTimeout(() => {
        store.dispatch(hideNotification(id))
      }, 5000)
    }
    
    // component.js
    showNotificationWithTimeout('You just logged in.')
    
    // otherComponent.js
    showNotificationWithTimeout('You just logged out.')    
    

This looks simpler but **we don’t recommend this approach**. The main reason we dislike it is because **it forces store to be a singleton**. This makes it very hard to implement [server rendering](http://redux.js.org/docs/recipes/ServerRendering.html). On the server, you will want each request to have its own store, so that different users get different preloaded data.

A singleton store also makes testing harder. You can no longer mock a store when testing action creators because they reference a specific real store exported from a specific module. You can’t even reset its state from outside.

So while you technically can export a singleton store from a module, we discourage it. Don’t do this unless you are sure that your app will never add server rendering.

Getting back to the previous version:

    // actions.js
    
    // ...
    
    let nextNotificationId = 0
    export function showNotificationWithTimeout(dispatch, text) {
      const id = nextNotificationId++
      dispatch(showNotification(id, text))
    
      setTimeout(() => {
        dispatch(hideNotification(id))
      }, 5000)
    }
    
    // component.js
    showNotificationWithTimeout(this.props.dispatch, 'You just logged in.')
    
    // otherComponent.js
    showNotificationWithTimeout(this.props.dispatch, 'You just logged out.')    
    

This solves the problems with duplication of logic and saves us from race conditions.

Thunk Middleware
----------------

For simple apps, the approach should suffice. Don’t worry about middleware if you’re happy with it.

In larger apps, however, you might find certain inconveniences around it.

For example, it seems unfortunate that we have to pass `dispatch` around. This makes it trickier to [separate container and presentational components](https://medium.com/@dan_abramov/smart-and-dumb-components-7ca2f9a7c7d0) because any component that dispatches Redux actions asynchronously in the manner above has to accept `dispatch` as a prop so it can pass it further. You can’t just bind action creators with `connect()` anymore because `showNotificationWithTimeout()` is not really an action creator. It does not return a Redux action.

In addition, it can be awkward to remember which functions are synchronous action creators like `showNotification()` and which are asynchronous helpers like `showNotificationWithTimeout()`. You have to use them differently and be careful not to mistake them with each other.

This was the motivation for **finding a way to “legitimize” this pattern of providing `dispatch` to a helper function, and help Redux “see” such asynchronous action creators as a special case of normal action creators** rather than totally different functions.

If you’re still with us and you also recognize as a problem in your app, you are welcome to use the [Redux Thunk](http://github.com/gaearon/redux-thunk) middleware.

In a gist, Redux Thunk teaches Redux to recognize special kinds of actions that are in fact functions:

    import { createStore, applyMiddleware } from 'redux'
    import thunk from 'redux-thunk'
    
    const store = createStore(
      reducer,
      applyMiddleware(thunk)
    )
    
    // It still recognizes plain object actions
    store.dispatch({ type: 'INCREMENT' })
    
    // But with thunk middleware, it also recognizes functions
    store.dispatch(function (dispatch) {
      // ... which themselves may dispatch many times
      dispatch({ type: 'INCREMENT' })
      dispatch({ type: 'INCREMENT' })
      dispatch({ type: 'INCREMENT' })
    
      setTimeout(() => {
        // ... even asynchronously!
        dispatch({ type: 'DECREMENT' })
      }, 1000)
    })
    

When this middleware is enabled, **if you dispatch a function**, Redux Thunk middleware will give it `dispatch` as an argument. It will also “swallow” such actions so don’t worry about your reducers receiving weird function arguments. Your reducers will only receive plain object actions—either emitted directly, or emitted by the functions as we just described.

This does not look very useful, does it? Not in this particular situation. However it lets us declare `showNotificationWithTimeout()` as a regular Redux action creator:

    // actions.js
    function showNotification(id, text) {
      return { type: 'SHOW_NOTIFICATION', id, text }
    }
    function hideNotification(id) {
      return { type: 'HIDE_NOTIFICATION', id }
    }
    
    let nextNotificationId = 0
    export function showNotificationWithTimeout(text) {
      return function (dispatch) {
        const id = nextNotificationId++
        dispatch(showNotification(id, text))
    
        setTimeout(() => {
          dispatch(hideNotification(id))
        }, 5000)
      }
    }
    

Note how the function is almost identical to the one we wrote in the previous section. However it doesn’t accept `dispatch` as the first argument. Instead it _returns_ a function that accepts `dispatch` as the first argument.

How would we use it in our component? Definitely, we could write this:

    // component.js
    showNotificationWithTimeout('You just logged in.')(this.props.dispatch)
    

We are calling the async action creator to get the inner function that wants just `dispatch`, and then we pass `dispatch`.

However this is even more awkward than the original version! Why did we even go that way?

Because of what I told you before. **If Redux Thunk middleware is enabled, any time you attempt to dispatch a function instead of an action object, the middleware will call that function with `dispatch` method itself as the first argument**.

So we can do this instead:

    // component.js
    this.props.dispatch(showNotificationWithTimeout('You just logged in.'))
    

Finally, dispatching an asynchronous action (really, a series of actions) looks no different than dispatching a single action synchronously to the component. Which is good because components shouldn’t care whether something happens synchronously or asynchronously. We just abstracted that away.

Notice that since we “taught” Redux to recognize such “special” action creators (we call them [thunk](https://en.wikipedia.org/wiki/Thunk) action creators), we can now use them in any place where we would use regular action creators. For example, we can use them with `connect()`:

    // actions.js
    
    function showNotification(id, text) {
      return { type: 'SHOW_NOTIFICATION', id, text }
    }
    function hideNotification(id) {
      return { type: 'HIDE_NOTIFICATION', id }
    }
    
    let nextNotificationId = 0
    export function showNotificationWithTimeout(text) {
      return function (dispatch) {
        const id = nextNotificationId++
        dispatch(showNotification(id, text))
    
        setTimeout(() => {
          dispatch(hideNotification(id))
        }, 5000)
      }
    }
    
    // component.js
    
    import { connect } from 'react-redux'
    
    // ...
    
    this.props.showNotificationWithTimeout('You just logged in.')
    
    // ...
    
    export default connect(
      mapStateToProps,
      { showNotificationWithTimeout }
    )(MyComponent)
    

Reading State in Thunks
-----------------------

Usually your reducers contain the business logic for determining the next state. However, reducers only kick in after the actions are dispatched. What if you have a side effect (such as calling an API) in a thunk action creator, and you want to prevent it under some condition?

Without using the thunk middleware, you’d just do this check inside the component:

    // component.js
    if (this.props.areNotificationsEnabled) {
      showNotificationWithTimeout(this.props.dispatch, 'You just logged in.')
    }
    

However, the point of extracting an action creator was to centralize this repetitive logic across many components. Fortunately, Redux Thunk offers you a way to _read_ the current state of the Redux store. In addition to `dispatch`, it also passes `getState` as the second argument to the function you return from your thunk action creator. This lets the thunk read the current state of the store.

    let nextNotificationId = 0
    export function showNotificationWithTimeout(text) {
      return function (dispatch, getState) {
        // Unlike in a regular action creator, we can exit early in a thunk
        // Redux doesn’t care about its return value (or lack of it)
        if (!getState().areNotificationsEnabled) {
          return
        }
    
        const id = nextNotificationId++
        dispatch(showNotification(id, text))
    
        setTimeout(() => {
          dispatch(hideNotification(id))
        }, 5000)
      }
    }
    

Don’t abuse this pattern. It is good for bailing out of API calls when there is cached data available, but it is not a very good foundation to build your business logic upon. If you use `getState()` only to conditionally dispatch different actions, consider putting the business logic into the reducers instead.

Next Steps
----------

Now that you have a basic intuition about how thunks work, check out Redux [async example](http://redux.js.org/docs/introduction/Examples.html#async) which uses them.

You may find many examples in which thunks return Promises. This is not required but can be very convenient. Redux doesn’t care what you return from a thunk, but it gives you its return value from `dispatch()`. This is why you can return a Promise from a thunk and wait for it to complete by calling `dispatch(someThunkReturningPromise()).then(...)`.

You may also split complex thunk action creators into several smaller thunk action creators. The `dispatch` method provided by thunks can accept thunks itself, so you can apply the pattern recursively. Again, this works best with Promises because you can implement asynchronous control flow on top of that.

For some apps, you may find yourself in a situation where your asynchronous control flow requirements are too complex to be expressed with thunks. For example, retrying failed requests, reauthorization flow with tokens, or a step-by-step onboarding can be too verbose and error-prone when written this way. In this case, you might want to look at more advanced asynchronous control flow solutions such as [Redux Saga](https://github.com/yelouafi/redux-saga) or [Redux Loop](https://github.com/raisemarketplace/redux-loop). Evaluate them, compare the examples relevant to your needs, and pick the one you like the most.

Finally, don’t use anything (including thunks) if you don’t have the genuine need for them. Remember that, depending on the requirements, your solution might look as simple as

    store.dispatch({ type: 'SHOW_NOTIFICATION', text: 'You logged in.' })
    setTimeout(() => {
      store.dispatch({ type: 'HIDE_NOTIFICATION' })
    }, 5000)
    

Don’t sweat it unless you know why you’re doing this.
    